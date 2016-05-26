/*
 *  Resample.scala
 *  (anemone_15b3c-video)
 *
 *  Copyright (c) 2015-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.anemone

import java.awt.Color
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.{dsp, numbers}
import de.sciss.file._
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl

import scala.concurrent.{ExecutionContext, blocking}

object Resample {
  case class Config(in: File = file("in"), out: File = file("out"),
                    startFrame: Int = 0, endFrame: Int = 0, factor: Int = 2,
                    noise: Double = 0.1,
                    resampleWindow: Int = 29, dropFrame: Int = 16, dropRate: Double = 0.0)

  def main(args: Array[String]): Unit = {
    val p = new scopt.OptionParser[Config]("Anemone Convolve") {
      arg[File]("input")
        .text ("Input image template (where %d will be replaced by frame index)")
        .required()
        .action { (f, c) => c.copy(in = f) }

      arg[File]("output")
        .text ("Output image template (where %d will be replaced by frame index)")
        .required()
        .action { (f, c) => c.copy(out = f) }

      opt[Int] ('s', "start-frame")
        .text ("Start frame index")
        .action   { (v, c) => c.copy(startFrame = v) }
        .validate {  v     => if (v >= 0) success else failure("start-frame must be >= 0") }

      opt[Int] ('e', "end-frame")
        .text ("End frame index")
        .action   { (v, c) => c.copy(endFrame = v) }
        .validate {  v     => if (v >= 0) success else failure("end-frame must be >= 0") }

      opt[Int] ('f', "factor")
        .text ("Resample factor (integer). Default: 2")
        .action   { (v, c) => c.copy(factor = v) }
        .validate {  v     => if (v >= 2) success else failure("factor must be >= 2") }

      opt[Double] ('n', "noise")
        .text ("Noise amplitude. Default: 0.1)")
        .action   { (v, c) => c.copy(noise = v) }
    }
    p.parse(args, Config()).fold(sys.exit(1)) { config =>
      val proc = new RenderImageSequence(config)
      import ExecutionContext.Implicits.global
      proc.start()
      proc.monitor()
      val sync = new AnyRef
      new Thread {
        override def run(): Unit = {
          sync.synchronized(sync.wait())
          Thread.sleep(1000)
          sys.exit()
        }
        start()
      }
      proc.onComplete { _ =>
        sync.synchronized(sync.notifyAll())
      }
    }
  }

  private def extractChannel(in: BufferedImage, w: Int, h: Int, chan: Int): Array[Array[Float]] = {
    val shift = chan * 8
    val iw = in.getWidth
    val ih = in.getHeight
    Array.tabulate(h) { y =>
      Array.tabulate(w) { x =>
        val i = if (x < iw && y < ih) (in.getRGB(x, y) >>> shift) & 0xFF else 0
        i.toFloat / 0xFF
      }
    }
  }

  def fillChannel(in: Array[Array[Float]], out: BufferedImage, chan: Int, add: Double = 0.0, mul: Double = 1.0): Unit = {
    val shift = chan * 8
    val mask  = ~(0xFF << shift)
    for (y <- in.indices) {
      val v = in(y)
      for (x <- v.indices) {
        val d = (v(x) + add) * mul
        import numbers.Implicits._
        val i = (d.clip(0, 1) * 0xFF + 0.5).toInt << shift
        val j = out.getRGB(x, y)
        val k = j & mask | i
        out.setRGB(x, y, k)
      }
    }
  }

  private def mkFIn(config: Config, frame: Int): File = {
    import config.in
    val name = in.name.format(frame)
    in.parentOption.fold(file(name))(_/name)
  }

  /** @param width    image width in pixels
    * @param height   image height in pixels
    * @param data     image data, as [chan][y][x]
    */
  private final class Frame(val width: Int, val height: Int, val data: Array[Array[Array[Float]]])

  private def readFrame(config: Config, frame: Int): Frame = {
    val fIn1  = mkFIn(config, frame)
    val img   = ImageIO.read(fIn1)
    val w     = img.getWidth
    val h     = img.getHeight
    val data  = Array.tabulate(3)(chan => extractChannel(img, w = w, h = h, chan = chan))
    new Frame(width = w, height = h, data = data)
  }

  private final class RenderImageSequence(config: Config)
    extends ProcessorImpl[Unit, RenderImageSequence] with Processor[Unit] {

    protected def body(): Unit = blocking {
      import config._

      val fmtOut = out.ext.toLowerCase

      val frameInMul    = if (endFrame >= startFrame) 1 else -1
      val frameSeq0     = startFrame to endFrame by frameInMul
      val frameSeq      = if (dropRate <= 0) frameSeq0 else {
        frameSeq0.filterNot { frame =>
          val x     = ((frame - dropFrame) / dropRate + 0.5).toInt
          val drop  = (x * dropRate + dropFrame + 0.5).toInt
          frame == drop
        }
      }

      val numInFrames   = frameSeq.size // math.abs(lastFrame - firstFrame + 1)
      // val frameOff      = firstFrame // if (lastFrame >= firstFrame) firstFrame else lastFrame
      val numOutFrames  = numInFrames * factor

      def mkFOut(frame: Int): File = {
        val name = out.name.format(frame)
        out.parentOption.fold(file(name))(_/name)
      }

      // e.g. resampleWindow = 5, winH = 2 ; L-L-L-R-R
      val winH = resampleWindow / 2

      var frame0      = readFrame(config, frameSeq(0) /* frameOff */)
      val widthIn     = frame0.width
      val heightIn    = frame0.height
      val imgOut      = new BufferedImage(widthIn, heightIn, BufferedImage.TYPE_INT_ARGB)
      // because we never override the alpha channel,
      // we must make sure it's opaque first:
      val gTmp = imgOut.createGraphics()
      gTmp.setColor(Color.black)
      gTmp.fillRect(0, 0, widthIn, heightIn)
      gTmp.dispose()

      // assert (widthIn == sizeIn && heightIn == sizeIn)

      val frameWindow = Array.tabulate(resampleWindow) { i =>
        val j = i - winH
        if (j <= 0) frame0 else readFrame(config, frameSeq(j) /* j * frameInMul + frameOff*/)
      }

      frame0 = null // let it be GC'ed

      val resample      = dsp.Resample(dsp.Resample.Quality.Medium /* Low */)
      val imgResample   = Array.fill(factor) {
        val data = Array.ofDim[Float](3, heightIn, widthIn)
        new Frame(width = widthIn, height = heightIn, data = data)
      }
      val bufResampleIn = new Array[Float](resampleWindow)
      val bufResampleOut= new Array[Float](factor)

      def performResample(): Unit = {
        var y = 0
        while (y < heightIn) {
          var x = 0
          while (x < widthIn) {
            var chan = 0
            while (chan < 3) {
              var t = 0
              while (t < resampleWindow) {
                val vIn = frameWindow(t).data(chan)(y)(x) // .getRGB(x, y)
                bufResampleIn(t) = vIn
                t += 1
              }
              resample.process(src = bufResampleIn, srcOff = winH, dest = bufResampleOut, destOff = 0,
                length = factor, factor = factor)
              var off = 0
              while (off < factor) {
                // note: gain factor 2 here!
                // val vOut    = (math.max(0f, math.min(1f, bufResampleOut(off) * factor)) * 255 + 0.5f).toInt
                val vOut    = math.max(0f, math.min(1f, bufResampleOut(off) * factor))
                imgResample(off).data(chan)(y)(x) = vOut // setRGB(x, y, rgbOut)
                off += 1
              }
              chan += 1
            }
            x += 1
          }
          y += 1
        }
      }

      var frameIn  = resampleWindow - winH
      var frameOut = 0
      while (frameOut < numOutFrames) {
        // val fOut1 = mkFOut(frameOut /* + 1 */)
        // val fOut2 = mkFOut(frameOut + 2)

        val fOutS = (frameOut until (frameOut + factor)).map(mkFOut)

        if (fOutS.exists(!_.exists())) {
          performResample()
          var off = 0
          while (off < factor) {
            val res  = imgResample(off)
            val fOut = fOutS(off)

            if (noise > 0.0) {
              var x = 0
              while (x < widthIn) {
                var y = 0
                while (y < heightIn) {
                  import numbers.Implicits._
                  val noise = math.random.linlin(0, 1, -config.noise, config.noise).toFloat
                  var chan = 0
                  while (chan < 3) {
                    res.data(chan)(y)(x) += noise
                    chan += 1
                  }
                  y += 1
                }
                x += 1
              }
            }

            var chan = 0
            while (chan < 3) {
              fillChannel(in = res.data(chan), out = imgOut, chan = chan, add = 0.0, mul = 1.0)
              chan += 1
            }
            ImageIO.write(imgOut, fmtOut, fOut)
            off += 1
          }
        }

        // handle overlap
        System.arraycopy(frameWindow, 1, frameWindow, 0, resampleWindow - 1)
        if (frameIn < numInFrames) {
          frameWindow(resampleWindow - 1) = readFrame(config, frameSeq(frameIn) /* frameIn * frameInMul + frameOff */)
        }

        frameIn  += 1
        frameOut += factor
        progress = frameIn.toDouble / numInFrames
        checkAborted()
      }
    }
  }
}

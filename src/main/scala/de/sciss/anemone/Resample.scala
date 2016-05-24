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

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.dsp
import de.sciss.file._
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl

import scala.concurrent.{ExecutionContext, blocking}

object Resample {
  case class Config(in: File = file("in"), out: File = file("out"),
                    startFrame: Int = 0, endFrame: Int = 0,
                    noise: Double = 1.0,
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

      opt[Double] ('n', "noise")
        .text ("Noise amplitude. Default: 1.0)")
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

  private def mkFIn(config: Config, frame: Int): File = {
    import config.in
    val name = in.name.format(frame)
    in.parentOption.fold(file(name))(_/name)
  }

  private def readFrame(config: Config, frame: Int): BufferedImage = {
    val fIn1      = mkFIn(config, frame)
    val imgIn     = ImageIO.read(fIn1)
    val imgCrop   = imgIn // cropImage2(config, imgIn)
    imgCrop
  }

  private final class RenderImageSequence(config: Config)
    extends ProcessorImpl[Unit, RenderImageSequence] with Processor[Unit] {

    protected def body(): Unit = blocking {
      import config._

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
      val numOutFrames  = numInFrames * 2
      // val imgOut        = new BufferedImage(sizeOut, sizeOut, BufferedImage.TYPE_BYTE_BINARY)

      def mkFOut(frame: Int): File = {
        val name = out.name.format(frame)
        out.parentOption.fold(file(name))(_/name)
      }

      // e.g. resampleWindow = 5, winH = 2 ; L-L-L-R-R
      val winH = resampleWindow / 2

      var frame0      = readFrame(config, frameSeq(0) /* frameOff */)
      val widthIn     = frame0.getWidth
      val heightIn    = frame0.getHeight

      // assert (widthIn == sizeIn && heightIn == sizeIn)

      val frameWindow = Array.tabulate(resampleWindow) { i =>
        val j = i - winH
        if (j <= 0) frame0 else readFrame(config, frameSeq(j) /* j * frameInMul + frameOff*/)
      }

      frame0 = null // let it be GC'ed

      val resample      = dsp.Resample(dsp.Resample.Quality.Medium /* Low */)
      val imgResample   = Array.fill(2)(new BufferedImage(widthIn, heightIn, BufferedImage.TYPE_BYTE_GRAY))
      val bufResampleIn = new Array[Float](resampleWindow)
      val bufResampleOut= new Array[Float](2)

      def performResample(): Unit = {
        var y = 0
        while (y < heightIn) {
          var x = 0
          while (x < widthIn) {
            var t = 0
            while (t < resampleWindow) {
              val rgbIn = frameWindow(t).getRGB(x, y)
              val vIn = (((rgbIn & 0xFF0000) >> 16) + ((rgbIn & 0x00FF00) >> 8) + (rgbIn & 0x0000FF)) / 765f // it's gray anyway
              bufResampleIn(t) = vIn
              t += 1
            }
            resample.process(src = bufResampleIn, srcOff = winH, dest = bufResampleOut, destOff = 0, length = 2, factor = 2)
            var off = 0
            while (off < 2) {
              // note: gain factor 2 here!
              val vOut    = (math.max(0f, math.min(1f, bufResampleOut(off) * 2)) * 255 + 0.5f).toInt
              val rgbOut  = 0xFF000000 | (vOut << 16) | (vOut << 8) | vOut
              imgResample(off).setRGB(x, y, rgbOut)
              off += 1
            }
            x += 1
          }
          y += 1
        }
      }

      var frameIn  = resampleWindow - winH
      var frameOut = 0
      while (frameOut < numOutFrames) {
        val fOut1 = mkFOut(frameOut + 1)
        val fOut2 = mkFOut(frameOut + 2)

        if (!fOut1.exists() || !fOut2.exists()) {
          performResample()
          var off = 0
          while (off < 2) {
            val imgCrop   = imgResample(off)
            val imgUp     = imgCrop // mkResize(config, imgCrop)
            val imgNoise  = imgUp // mkNoise(config, imgUp)
            // mkThresh(config, imgNoise, imgOut)
            ImageIO.write(imgNoise /* imgOut */, "png", if (off == 0) fOut1 else fOut2)
            off += 1
          }
        }

        // handle overlap
        System.arraycopy(frameWindow, 1, frameWindow, 0, resampleWindow - 1)
        if (frameIn < numInFrames) {
          frameWindow(resampleWindow - 1) = readFrame(config, frameSeq(frameIn) /* frameIn * frameInMul + frameOff */)
        }

        frameIn  += 1
        frameOut += 2
        progress = frameIn.toDouble / numInFrames
        checkAborted()
      }
    }
  }
}

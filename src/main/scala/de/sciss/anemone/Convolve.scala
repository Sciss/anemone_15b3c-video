package de.sciss.anemone

import java.awt.Color
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import de.sciss.dsp
import de.sciss.file._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFile
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D

object Convolve extends App {
  case class Config(inA: File = file("in-a"), inB: File = file("in-b"), out: File = file("out"),
                    startFrame: Int = 0, endFrame: Int = 0,
                    frameOffA: Int = 0, frameOffB: Int = 0, gamma: Double = 0.5, kernel: Int = 32,
                    agcLag: Double = 0.9, noise: Double = 1.0, winSize: Int = 4, rotateB: Boolean = false,
                    shiftB: Int = 0, filter: Option[File] = None,
                    width: Int = 0, height: Int = 0)

  val p = new scopt.OptionParser[Config]("Anemone Convolve") {
    arg[File]("input-a")
      .text ("First input image template (where %d will be replaced by frame index)")
      .required()
      .action { (f, c) => c.copy(inA = f) }

    arg[File]("input-b")
      .text ("Second input image template (where %d will be replaced by frame index)")
      .required()
      .action { (f, c) => c.copy(inB = f) }

    arg[File]("output")
      .text ("Output image template (where %d will be replaced by frame index)")
      .required()
      .action { (f, c) => c.copy(out = f) }

    opt[File]('f', "filter")
      .text ("High pass filter (sound file)")
      .action { (f, c) => c.copy(filter = Some(f)) }

    opt[Int] ('s', "start-frame")
      .text ("Start frame index")
      .action   { (v, c) => c.copy(startFrame = v) }
      .validate {  v     => if (v >= 0) success else failure("start-frame must be >= 0") }

    opt[Int] ('e', "end-frame")
      .text ("End frame index")
      .action   { (v, c) => c.copy(endFrame = v) }
      .validate {  v     => if (v >= 0) success else failure("end-frame must be >= 0") }

    opt[Int] ("offset-a")
      .text ("Frame index offset for first image")
      .action   { (v, c) => c.copy(frameOffA = v) }

    opt[Int] ("offset-b")
      .text ("Frame index offset for second image")
      .action   { (v, c) => c.copy(frameOffB = v) }

    opt[Double] ('g', "gamma")
      .text ("Gamma correction (< 1 darker, > 1 brighter, default: 0.5")
      .action   { (v, c) => c.copy(gamma = v) }
      .validate { v => if (v > 0) success else failure("gamma must be > 0") }

    opt[Int] ('k', "kernel")
      .text ("Convolution kernel size. Must be a power of two. Default: 32")
      .action   { (v, c) => c.copy(kernel = v) }
      .validate {  v     => if (v >= 2 && v.isPowerOfTwo) success else failure("kernel must be >= 2 and power of two") }

    opt[Double] ('l', "gain-lag")
      .text ("ACG lag coefficient (0.0 to 1.0, default: 0.9)")
      .action   { (v, c) => c.copy(agcLag = v) }
      .validate {  v     => if (v >= 0 && v <= 1.0) success else failure("gain-lag be >= 0 and <= 1") }

    opt[Double] ('n', "noise")
      .text ("Noise amplitude. Default: 1.0)")
      .action   { (v, c) => c.copy(noise = v) }

    opt[Int] ('i', "window")
      .text ("Window size (must be >= 2; default: 4)")
      .action   { (v, c) => c.copy(winSize = v) }
      .validate {  v     => if (v >= 2) success else failure("window must be >= 2") }

    opt[Int] ('w', "width")
      .text ("Enforce image width")
      .action   { (v, c) => c.copy(width = v) }

    opt[Int] ('h', "height")
      .text ("Enforce image heiht")
      .action   { (v, c) => c.copy(height = v) }

    opt[Unit] ('r', "rotate")
      .text ("Rotate second image")
      .action   { (v, c) => c.copy(rotateB = true) }

    opt[Int] ('y', "shift-y")
      .text ("Vertical offset of second image. Default: 0")
      .action   { (v, c) => c.copy(shiftB = v) }
  }
  p.parse(args, Config()).fold(sys.exit(1)) { config =>
    new Convolve(config)
  }

  final case class Gain(min: Double, max: Double)
}
final class Convolve(config: Convolve.Config) {
  import Convolve.Gain

  def extractChannel(in: BufferedImage, chan: Int): Array[Array[Double]] = {
    val shift = chan * 8
    Array.tabulate(in.getHeight) { y =>
      Array.tabulate(in.getWidth) { x =>
        val i = (in.getRGB(x, y) >>> shift) & 0xFF
        i.toDouble / 0xFF
      }
    }
  }

  def extractChannel1(in: BufferedImage, w: Int, h: Int, chan: Int): Array[Array[Double]] = {
    val shift = chan * 8
    val iw = in.getWidth
    val ih = in.getHeight
    Array.tabulate(h) { y =>
      Array.tabulate(w) { x =>
        val i = if (x < iw && y < ih) (in.getRGB(x, y) >>> shift) & 0xFF else 0
        i.toDouble / 0xFF
      }
    }
  }

  def extractChannel2(in: BufferedImage, arr: Array[Array[Double]], x: Int, y: Int, w: Int, h: Int, chan: Int): Unit = {
    val shift = chan * 8
    val iw = in.getWidth
    val ih = in.getHeight
    var y0 = 0
    while (y0 < h) {
      var x0 = 0
      val arr0 = arr(y0)
      while (x0 < w) {
        val x1  = x0 + x
        val y1  = y0 + y
        val i   = if (x1 >= 0 && y1 >= 0 && x1 < iw && y1 < ih) (in.getRGB(x1, y1) >>> shift) & 0xFF else 0
        val d = i.toDouble / 0xFF
        arr0(x0) = d
        x0 += 1
      }
      y0 += 1
    }
  }

  def fillChannel(in: Array[Array[Double]], out: BufferedImage, chan: Int, add: Double = 0.0, mul: Double = 1.0): Unit = {
    val shift = chan * 8
    val mask  = ~(0xFF << shift)
    for (y <- in.indices) {
      val v = in(y)
      for (x <- v.indices) {
        val d = (v(x) + add) * mul
        val i = (d.clip(0, 1) * 0xFF + 0.5).toInt << shift
        val j = out.getRGB(x, y)
        val k = j & mask | i
        out.setRGB(x, y, k)
      }
    }
  }

  def mulC(a: Array[Array[Double]], b: Array[Array[Double]], scale: Double = 1.0): Unit = {
    for (y <- a.indices) {
      val va = a(y)
      val vb = b(y)
      for (x <- va.indices by 2) {
        val re = va(x) * vb(x)   - va(x+1) * vb(x+1)
        val im = va(x) * vb(x+1) + va(x+1) * vb(x)
        va(x)   = re * scale
        va(x+1) = im * scale
      }
    }
  }

  import config.{kernel, winSize}

  private[this] val fft       = new DoubleFFT_2D(kernel, kernel)
  private[this] val gammaInv  = 1.0/config.gamma
  private[this] val imgFormat = config.out.ext.toLowerCase
  private[this] val kh        = kernel/2

  def levels(in: Double): Double = {
    val clip = in.clip(0, 1)
    val easy = clip.linlin(0, 1, math.Pi/2, 0).cos.squared  // 'easy in easy out'
    easy.pow(gammaInv)
  }

  val bFltOpt = config.filter.map { filtF =>
    val af  = AudioFile.openRead(filtF)
    val afb = af.buffer(af.numFrames.toInt)
    af.read(afb)
    af.close()
    val flt = afb(0)

    val fltLen  = flt.length
    val fltLenH = fltLen >> 1
    val bFlt = Array.tabulate(kernel) { y =>
      Array.tabulate(kernel) { x =>
        val x0 = (if (x > kh) x - kernel else x) + fltLenH
        val y0 = (if (y > kh) y - kernel else y) + fltLenH
        if (x0 >= 0 && y0 >= 0 && x0 < fltLen && y0 < fltLen)
          flt(x0) * flt(y0)
        else
          0.0
      }
    }
    fft.realForward(bFlt)
    bFlt
  }

  private[this] val b1 = Array.ofDim[Double](kernel, kernel)
  private[this] val b2 = Array.ofDim[Double](kernel, kernel)

  private[this] val wh = winSize/2

  private[this] val win = dsp.Window.Hanning.fill(null, 0, winSize)

  def renderFrame(fInA: File, fInB: File, fOut: File, agc: Double, prevGain: Gain): Gain = {
    val i1  = ImageIO.read(fInA)
    val w   = if (config.width  > 0) config.width  else i1.getWidth
    val h   = if (config.height > 0) config.height else i1.getHeight
    val i2a = ImageIO.read(fInB)
    val i2  = if (!config.rotateB && config.shiftB == 0) i2a else {
      val res   = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      val gTmp1 = res.createGraphics()
      val atT = if (config.rotateB)
        AffineTransform.getRotateInstance(math.Pi, i2a.getWidth/2, i2a.getHeight/2 + config.shiftB)
      else
        AffineTransform.getTranslateInstance(0, config.shiftB)
      gTmp1.drawImage(i2a, atT, null)
      gTmp1.dispose()
      res
    }

    val i3 = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val gTmp = i3.createGraphics()
    gTmp.setColor(Color.black)
    gTmp.fillRect(0, 0, w, h)
    gTmp.dispose()

    val bCh0 = Array.ofDim[Double](3, h, w)

    println("_" * 40)

    var lastProg  = 0
    val w1        = w - winSize
    val h1        = h - winSize
    val progScale = 40.0 / (w1 * h1)

    for (x <- 0 until w1 by wh) {
      for (y <- 0 until h1 by wh) {
        // val noise = math.random.linlin(0, 1, -config.noise, config.noise)
        for (chan <- 0 until 3) {
          val b3 = bCh0(chan)
          extractChannel2(i1, arr = b1, x = x - kh, y = y - kh, w = kernel, h = kernel, chan = chan)
          extractChannel2(i2, arr = b2, x = x - kh, y = y - kh, w = kernel, h = kernel, chan = chan)
          fft.realForward(b1)
          fft.realForward(b2)
          mulC(b1, b2, scale = 1.0)
          bFltOpt.foreach { bFlt =>
            mulC(b1, bFlt, scale = 1.0)
          }
          fft.realInverse(b1, true)

          if (wh == 1) {
             val d = b1(0)(0)
             b3(y)(x) = d // + noise
          } else {
            var xi = 0
            while (xi < winSize) {
              var yi = 0
              while (yi < winSize) {
                val d = b1(yi)(xi) * win(yi) * win(xi)
                b3(y + yi)(x + xi) += d
                yi += 1
              }
              xi += 1
            }
          }
        }

        val prog = ((x * h1 + y + 1) * progScale).toInt
        while (lastProg < prog) {
          print('#')
          lastProg += 1
        }
      }
    }
    while (lastProg < 40) {
      print('#')
      lastProg += 1
    }

    val bCh = bCh0 // .map(_.clone())

    val min0 = bCh.map(b1 => b1.map(_.min).min).min
    val max0 = bCh.map(b1 => b1.map(_.max).max).max  // = max(max(red), max(green), max(blue))
    val min  = min0 * (1 - agc) + prevGain.min * agc
    val max  = max0 * (1 - agc) + prevGain.max * agc
    val mul  = if (max == min) 1.0 else 1.0/(max-min)
    val add  = if (max == min) 0.0 else -min

    for (x <- 0 until w) {
      for (y <- 0 until h) {
        val noise = math.random.linlin(0, 1, -config.noise, config.noise)
        for (chan <- 0 until 3) {
          val row = bCh0(chan)(y)
          val a = (row(x) + add + noise) * mul
          row(x) = levels(a)
        }
      }
    }

    bCh.zipWithIndex.foreach { case (bChan, chan) =>
      fillChannel(bChan, i3 /* i1 */, chan = chan /* , add = add, mul = mul */)
    }

    ImageIO.write(i3 /* i1 */, imgFormat, fOut)

    Gain(min = min, max = max)
  }

  import config._
  private[this] var _gain = Gain(0, 0)
  for (frame <- startFrame to endFrame) {
    def format(f: File, i: Int): File = {
      val name = f.name.format(i)
      f.parentOption.fold(file(name))(_/name)
    }

    println(s"${new java.util.Date()} : ----- frame $frame -----")
    val fInA = format(inA, frame + frameOffA)
    val fInB = format(inB, frame + frameOffB)
    val fOut = format(out, frame)
    if (!fOut.exists()) {
      val agc = if (frame == startFrame) 0.0 else config.agcLag
      _gain = renderFrame(fInA = fInA, fInB = fInB, fOut = fOut, agc = agc, prevGain = _gain)
      println()
    }
  }
  sys.exit()
}
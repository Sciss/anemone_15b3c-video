package de.sciss.anemone

import java.awt.Color
import java.awt.geom.AffineTransform

import edu.emory.mathcs.jtransforms.fft.DoubleFFT_2D
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import de.sciss.file._
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration.Duration

object Convolve extends App {
  val outDir  = file("conv_image_out")
  val inDir1  = userHome / "Documents"/"projects"/"Anemone"
  val inDir2  = userHome / "Documents"/"projects"/"Langzeitbelichtung"
  // val p1      = inDir1 / "burned4"/"video"/"anemone-150916-1cm.jpg"
  val p1      = inDir1 / "burned4"/"video"/"vlcsnap-2015-09-16-1.png"
  // val p2      = inDir2 / "material"/"frame-245_crop.jpg"
  val p2      = inDir2 / "material"/"frame-245.png"
  val pOut    = outDir/ "out.png"

  if (!outDir.exists()) outDir.mkdirs()
  if (pOut.exists()) {
    println(s"Output file '$pOut' already exists. Not overwriting.")
    sys.exit(1)
  }

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

  def perform(): Unit = {
    val i1  = ImageIO.read(p1)
    val i2  = ImageIO.read(p2)

    val af  = AudioFile.openRead("filters/hp7.aif")
    val afb = af.buffer(af.numFrames.toInt)
    af.read(afb)
    af.close()
    val flt = afb(0)

    val w0 = i1.getWidth
    val h0 = i1.getHeight
    // require(w0 == i2.getWidth && h0 == i2.getHeight)
    // require(w0.isPowerOfTwo && h0.isPowerOfTwo)
    val w = w0.nextPowerOfTwo
    val h = h0.nextPowerOfTwo

    val fltLen  = flt.length
    val fltLenH = fltLen >> 1
    val bFlt = Array.tabulate(h) { y =>
      Array.tabulate(w) { x =>
        val x0 = (if (x > w/2) x - w else x) + fltLenH
        val y0 = (if (y > h/2) y - h else y) + fltLenH
        if (x0 >= 0 && y0 >= 0 && x0 < fltLen && y0 < fltLen)
          flt(x0) * flt(y0)
        else
          0.0
      }
    }

    val fft = new DoubleFFT_2D(h, w)

    val i3 = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val gTmp = i3.createGraphics()
    gTmp.setColor(Color.black)
    gTmp.fillRect(0, 0, w, h)
    gTmp.dispose()

    fft.realForward(bFlt)

    val bCh0 = (0 until 3).map { chan =>
//      val b1 = extractChannel(i1, chan)
//      val b2 = extractChannel(i2, chan)
      val b1 = extractChannel1(i1, w = w, h = h, chan = chan)
      val b2 = extractChannel1(i2, w = w, h = h, chan = chan)
      fft.realForward(b1)
      fft.realForward(b2)
      mulC(b1, b2  , scale = 1.0)
      mulC(b1, bFlt, scale = 1.0)
      fft.realInverse(b1, true)
      b1
    }

    val bCh = bCh0 // .map(_.clone())

//    // high pass
//    for (y1 <- 0 until h) {
//      for (x1 <- 0 until w) {
//        val x0 = (x1 - 1).wrap(0, w-1)
//        val x2 = (x1 + 1).wrap(0, w-1)
//        val y0 = (y1 - 1).wrap(0, h-1)
//        val y2 = (y1 + 1).wrap(0, h-1)
//
//        for (ch <- 0 until 3) {
//          val a = bCh0(ch)
//          val b = bCh (ch)
//          val c = a(y1)(x1)
//          val d =
//             a(y0)(x0)    + a(y0)(x1)    + a(y0)(x2) +
//             a(y1)(x0) /* + a(y1)(x1) */ + a(y1)(x2) +
//             a(y2)(x0)    + a(y2)(x1)    + a(y2)(x2)
//          val e = c - d/8
//          b(y1)(x1) = e
//        }
//      }
//    }

    val max = bCh.map(b1 => b1.map(_.max).max).max
    val gain = 1.0/max
    bCh.zipWithIndex.foreach { case (b1, chan) =>
      fillChannel(b1, i3 /* i1 */, chan = chan, mul = gain)
    }

    ImageIO.write(i3 /* i1 */, "png", pOut)
  }

  def perform1(): Unit = {
    val i1  = ImageIO.read(p1)
    val i2  = ImageIO.read(p2)

    val w0  = i1.getWidth
    val h0  = i1.getHeight
    val w   = w0.nextPowerOfTwo
    val h   = h0.nextPowerOfTwo

    val ins = Seq(i1, i2)

    for (i <- 0 until 2) {
      val i3 = ins(i)
      val bCh = Array(0 until 3: _*).map { chan =>
        val b1 = extractChannel1(i3, w = w, h = h, chan = chan)
        b1.flatMap(r => r.map(_.toFloat))
      }
      val spec = AudioFileSpec(numChannels = 3, sampleRate = 44100.0)
      val fOut = outDir / s"image-${i+1}.aif"
      require(!fOut.exists())
      val af = AudioFile.openWrite(fOut, spec)
      af.write(bCh, 0, bCh(0).length)
      af.close()
    }
  }

  def perform2(): Unit = {
    // val af = AudioFile.openRead(outDir / "image-conv.aif")
    // val af = AudioFile.openRead(outDir / "image-2White.aif")
    // val af = AudioFile.openRead(outDir / "image-op.aif")
    val af = AudioFile.openRead(outDir / "image-1Wrp.aif")
    val i1  = ImageIO.read(p1)
    val w0  = i1.getWidth
    val h0  = i1.getHeight
    val w   = w0.nextPowerOfTwo
    val h   = h0.nextPowerOfTwo
    val afb = af.buffer(af.numFrames.toInt)
    af.read(afb)

    val bCh = (0 until 3).map { chan =>
      afb(chan).map(_.toDouble).grouped(h).toArray
    }

    val i3 = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val gTmp = i3.createGraphics()
    gTmp.setColor(Color.black)
    gTmp.fillRect(0, 0, w, h)
    gTmp.dispose()

    val max = bCh.map(b1 => b1.map(_.max).max).max
    val gain = 1.0/max
    bCh.zipWithIndex.foreach { case (b1, chan) =>
      fillChannel(b1, i3 /* i1 */, chan = chan, mul = gain)
    }

    ImageIO.write(i3 /* i1 */, "png", pOut)
  }

  def perform3(): Unit = {
    val i1  = ImageIO.read(p1)
    val i2a = ImageIO.read(p2)
    val i2  = new BufferedImage(1920, 1080, BufferedImage.TYPE_INT_ARGB)
    val gTmp1 = i2.createGraphics()
    val atT = AffineTransform.getRotateInstance(math.Pi, 1920/2, 1080/2 + 64)
    gTmp1.drawImage(i2a, atT, null)
    gTmp1.dispose()

    val kernelSize = 32
    val kh = kernelSize/2

    val af  = AudioFile.openRead("filters/hp6.aif")
    val afb = af.buffer(af.numFrames.toInt)
    af.read(afb)
    af.close()
    val flt = afb(0)

    val w0 = i1.getWidth
    val h0 = i1.getHeight
    // require(w0 == i2.getWidth && h0 == i2.getHeight)
    // require(w0.isPowerOfTwo && h0.isPowerOfTwo)
    val w = w0 // 512 // 256 // w0 // .nextPowerOfTwo
    val h = h0 // 512 // 256 // h0 // .nextPowerOfTwo

    val fltLen  = flt.length
    val fltLenH = fltLen >> 1
    val bFlt = Array.tabulate(kernelSize) { y =>
      Array.tabulate(kernelSize) { x =>
        val x0 = (if (x > kh) x - kernelSize else x) + fltLenH
        val y0 = (if (y > kh) y - kernelSize else y) + fltLenH
        if (x0 >= 0 && y0 >= 0 && x0 < fltLen && y0 < fltLen)
          flt(x0) * flt(y0)
        else
          0.0
      }
    }

    // val fft = new DoubleFFT_2D(kernelSize, kernelSize)
    // val ffts = Array.fill(3)(new DoubleFFT_2D(kernelSize, kernelSize))
    val ffts = { val fft = new DoubleFFT_2D(kernelSize, kernelSize); Array.fill(3)(fft) }

    val i3 = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val gTmp = i3.createGraphics()
    gTmp.setColor(Color.black)
    gTmp.fillRect(0, 0, w, h)
    gTmp.dispose()

    ffts(0).realForward(bFlt)

    val bCh0 = Array.ofDim[Double](3, h, w)

    println("_" * 100)
    var lastProg = 0
    val progScale = 100.0 / (w * h)

    // val processorCount = Runtime.getRuntime.availableProcessors

    val bufs1 = Array.ofDim[Double](3, kernelSize, kernelSize)
    val bufs2 = Array.ofDim[Double](3, kernelSize, kernelSize)

    for (x <- 0 until w) {
      for (y <- 0 until h) {
        val noise = math.random.linlin(0, 1, -1, 1)
        // import ExecutionContext.Implicits.global
        /* val futures = */ (0 until 3).foreach /* map */ { chan =>
          // Future {
            // blocking {
              val b1 = bufs1(chan)
              val b2 = bufs2(chan)
              val fft = ffts(chan)
              extractChannel2(i1, arr = b1, x = x - kh, y = y - kh, w = kernelSize, h = kernelSize, chan = chan)
              extractChannel2(i2, arr = b2, x = x - kh, y = y - kh, w = kernelSize, h = kernelSize, chan = chan)
              fft.realForward(b1)
              fft.realForward(b2)
              mulC(b1, b2, scale = 1.0)
              mulC(b1, bFlt, scale = 1.0)
              fft.realInverse(b1, true)
              val d = b1(0)(0)
              bCh0(chan)(y)(x) = d + noise
            // }
          // }
        }
        // Await.result(Future.sequence(futures), Duration.Inf)

        val prog = ((x * h + y + 1) * progScale).toInt
        while (lastProg < prog) {
          print("#")
          lastProg += 1
        }
      }
    }

    val bCh = bCh0 // .map(_.clone())

    val min = bCh.map(b1 => b1.map(_.min).min).min
    val max = bCh.map(b1 => b1.map(_.max).max).max  // = max(max(red), max(green), max(blue))
    println(s"max = $max")
    // val gain = 1.0/max
    val mul  = 1.0/(max-min)
    val add  = -min

    def levels(in: Double, /* lo: Double, hi: Double, */ gamma: Double): Double = {
      val clip = in.clip(0, 1)
      // val clip = in.clip(lo, hi).linlin(lo, hi, 0, 1)
      val easy = clip.linlin(0, 1, math.Pi/2, 0).cos.squared // .linlin(0, 1, from, to)
      easy.pow(gamma)
    }

    bCh.foreach { b1 =>
      b1.foreach { row =>
        var i = 0
        while (i < row.length) {
          val a = (row(i) + add) * mul
          row(i) = levels(a, /* lo = 0.25, hi = 0.75, */ gamma = 2)
          i += 1
        }
      }
    }

    bCh.zipWithIndex.foreach { case (b1, chan) =>
      fillChannel(b1, i3 /* i1 */, chan = chan /* , add = add, mul = mul */)
    }

    ImageIO.write(i3 /* i1 */, "png", pOut)
  }

  perform3()
  sys.exit()
}
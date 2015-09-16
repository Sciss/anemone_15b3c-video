/*
 *  Video.scala
 *  (anemone_15b3c-video)
 *
 *  Copyright (c) 2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.anemone

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{Dimension, EventQueue}
import javax.swing.WindowConstants

import de.sciss.fscape.spect.Wavelet
import de.sciss.numbers
import processing.core.{PApplet, PConstants, PImage}
import processing.video.Capture

object Video {
  case class Config(device: String = "/dev/video0", listDevices: Boolean = false, fps: Int = 24)

  def main(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("anemone_15b3c-video") {
      opt[Unit  ]('l', "list"  ) text "list video capture devices" action { case (_, c) => c.copy(listDevices = true) }
      opt[String]('d', "device") text "video capture device" action { case (v, c) => c.copy(device = v) }
      opt[Int   ]('r', "fps"   ) text "frames per second" action { (v, c) => c.copy(fps = v) }
    }
    parser.parse(args, Config()).fold(sys.exit(1)) { config =>
      if (config.listDevices) {
        println("---- video devices found: ----")
        Capture.list().foreach(println)
        sys.exit()
      }
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = Video.run(config)
      })
    }
  }

  private def run(config: Config): Unit = {
    val sketch = new Video(config)
    val frame = new javax.swing.JFrame("Anemone")
    frame.getContentPane.add(sketch)
    sketch.init()
    frame.setUndecorated(true)
    frame.setResizable(false)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
    sketch.installFullScreenKey(frame)
  }
}
final class Video(config: Video.Config) extends PApplet {
  import numbers.Implicits._

  private[this] val VIDEO_WIDTH   = 1920
  private[this] val VIDEO_HEIGHT  = 1080
  private[this] val VIDEO_FPS     = config.fps // 24 // 30
  private[this] val VIDEO_DEVICE  = config.device
  // private[this] val VIDEO_SIZE    = VIDEO_WIDTH * VIDEO_HEIGHT

  private[this] val WINDOW_WIDTH  = 1024 // VIDEO_WIDTH  / 2
  private[this] val WINDOW_HEIGHT =  768 // VIDEO_HEIGHT / 2
  private[this] val WINDOW_SIZE   = WINDOW_WIDTH * WINDOW_HEIGHT
  private[this] val BUF_SIZE      = WINDOW_SIZE.nextPowerOfTwo

  private[this] val SCAN  = VIDEO_WIDTH - WINDOW_WIDTH

  private[this] var cam: Capture = _
  private[this] val buf1    = new Array[Float](BUF_SIZE)
  private[this] val buf2    = new Array[Float](BUF_SIZE)
  private[this] val imgOut  = new PImage(WINDOW_WIDTH, WINDOW_HEIGHT, PConstants.RGB /* ARGB */)

  private[this] val X_START1 = 0
  private[this] val Y_START1 = (VIDEO_HEIGHT - WINDOW_HEIGHT) / 2
  private[this] val X_START2 =  VIDEO_WIDTH  - WINDOW_WIDTH
  private[this] val Y_START2 = (VIDEO_HEIGHT - WINDOW_HEIGHT) / 2

  private[this] val WAVELET_4  = Wavelet.getCoeffs(Wavelet.COEFFS_DAUB4 )
  private[this] val WAVELET_16 = Wavelet.getCoeffs(Wavelet.COEFFS_DAUB16)

  override def init(): Unit = {
    super.init()
    setPreferredSize(new Dimension(WINDOW_WIDTH, WINDOW_HEIGHT))
  }

  override def setup(): Unit = {
    size(WINDOW_WIDTH, WINDOW_HEIGHT)
    cam = new Capture(this, VIDEO_WIDTH, VIDEO_HEIGHT, VIDEO_DEVICE, VIDEO_FPS)
    cam.start()
    noLoop()
    imgOut.loadPixels()
  }

  private[this] def crop(xStart: Int, yStart: Int, buf: Array[Float]): Unit = {
    val pixIn = cam.pixels

    // ---- copy grayscale image to buffer ----
    var i       = yStart * VIDEO_WIDTH + xStart
    var j       = 0
    while (j < WINDOW_SIZE) {
      val k = i + WINDOW_WIDTH
      while (i < k) {
        val rgbIn = pixIn(i)
        // pix(i) = pix(i) ^ 0xFFFFFF
        val red   = (rgbIn & 0xFF0000) >> 16
        val green = (rgbIn & 0x00FF00) >>  8
        val blue  =  rgbIn & 0x0000FF

        val bright = (0.299f * red + 0.587f * green + 0.114f * blue) / 0xFF
        // val bright = if (bright0 > 1f) 1f else bright0
        buf(j) = bright

        i += 1
        j += 1
      }
      i += SCAN
    }
    while (j < BUF_SIZE) {
      buf(j) = 0f
      j += 1
    }
  }

  def captureEvent(c: Capture): Unit = {
    c.read()

    crop(xStart = X_START1, yStart = Y_START1, buf = buf1)
    crop(xStart = X_START2, yStart = Y_START2, buf = buf2)

    Wavelet.fwdTransform(buf1, BUF_SIZE, WAVELET_4)
    Wavelet.fwdTransform(buf2, BUF_SIZE, WAVELET_4)

    var i = 0
    // var MIN = Float.PositiveInfinity
    // var MAX = Float.NegativeInfinity
    // val gain = (1.0f / BUF_SIZE).sqrt
    while (i < BUF_SIZE) {
      // if (buf1(i) > MAX) MAX = buf1(i)
      // if (buf1(i) < MIN) MIN = buf1(i)
      // buf1(i) *= buf2(i) * gain

      // buf1(i) = math.max(buf1(i), buf2(i))
      // buf1(i) = math.max(buf1(i), buf2(i)) - math.min(buf1(i), buf2(i))
      // buf1(i) = buf1(i).toInt ^ buf2(i).toInt
      // buf1(i) = (buf1(i) atan2 buf2(i)) * 0.25f
      // buf1(i) = (buf1(i) hypot buf2(i))
      // buf1(i) = (buf1(i) hypotx buf2(i))  // !
      if (i % 2 == 0) buf1(i) = buf2(i)
      i += 1
    }
    // println(s"MIN = $MIN, MAX = $MAX")

    Wavelet.invTransform(buf1, BUF_SIZE, WAVELET_4)

    //    Wavelet.fwdTransform(buf, BUF_SIZE, WAVELET_16)
//    Wavelet.invTransform(buf, BUF_SIZE, WAVELET_4 )

    // ---- normalize ----
    val NORMALIZE = false

    if (NORMALIZE) {
      i = 1
      var MIN = buf1(0)
      var MAX = buf1(0)
      while (i < WINDOW_SIZE) {
        val x = buf1(i)
        if (x < MIN) MIN = x
        if (x > MAX) MAX = x
        i += 1
      }
      // println(s"MIN = $MIN, MAX = $MAX")
      if (MIN < MAX) {
        i = 0
        val off   = -MIN
        val scale = 1.0f / (MAX - MIN)
        while (i < WINDOW_SIZE) {
          buf1(i) = (buf1(i) + off) * scale
          i += 1
        }
      }
    }

    val NOISE = 0.1f

    if (NOISE > 0f) {
      i = 1
      while (i < WINDOW_SIZE) {
        buf1(i) += (java.lang.Math.random().toFloat - 0.5f) * NOISE
        i += 1
      }
    }

    // ---- copy buffer to p-image ----
    val pixOut  = imgOut.pixels
    var j = 0
    while (j < WINDOW_SIZE) {
      val b0      = buf1(j)
      val b1      = if (b0 < 0f) 0f else b0
      val bright  = if (b1 > 1f) 1f else b1
      val bi      = (bright * 0xFF).toInt
      val rgbOut  = 0xFF000000 | (bi << 16) | (bi << 8) | bi
      pixOut(j)   = rgbOut
      j += 1
    }
    imgOut.updatePixels()
    redraw()
  }

  override def draw(): Unit = {
    // if (cam.available()) cam.read()
    val w = getWidth
    val h = getHeight
    fill(0f)
    rect(0, 0, w, h)
    val x = (w - WINDOW_WIDTH ) >> 1
    val y = (h - WINDOW_HEIGHT) >> 1
    image(imgOut /* cam */, x, y) // , WINDOW_WIDTH, WINDOW_HEIGHT)
  }

  def installFullScreenKey(frame: java.awt.Window): Unit = {
    addKeyListener(new KeyAdapter {
      override def keyPressed(e: KeyEvent): Unit = if (e.isControlDown) {
        if (e.getKeyCode == KeyEvent.VK_F && e.isShiftDown) {
          val gc = frame.getGraphicsConfiguration
          val sd = gc.getDevice
          sd.setFullScreenWindow(if (sd.getFullScreenWindow == frame) null else frame)
        } else if (e.getKeyCode == KeyEvent.VK_Q) {
          sys.exit()
        }
      }
    })
  }
}
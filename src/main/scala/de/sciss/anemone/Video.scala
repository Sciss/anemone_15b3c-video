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

import java.awt.{Dimension, EventQueue}
import javax.swing.WindowConstants

import de.sciss.fscape.spect.Wavelet
import de.sciss.numbers
import processing.core.{PConstants, PImage, PApplet}
import processing.video.Capture

object Video extends Runnable {
  def main(args: Array[String]) = EventQueue.invokeAndWait(this)

  def run(): Unit = {
    val sketch = new Video
    val frame = new javax.swing.JFrame("Anemone")
    frame.getContentPane.add(sketch)
    sketch.init()
    frame.setResizable(false)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }
}
final class Video extends PApplet {
  import numbers.Implicits._

  private[this] val VIDEO_WIDTH   = 1920
  private[this] val VIDEO_HEIGHT  = 1080
  private[this] val VIDEO_FPS     = 24 // 30
  private[this] val VIDEO_DEVICE  = "/dev/video1"
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
      buf1(i) = math.max(buf1(i), buf2(i))
      i += 1
    }
    // println(s"MIN = $MIN, MAX = $MAX")

    Wavelet.invTransform(buf1, BUF_SIZE, WAVELET_4)

    //    Wavelet.fwdTransform(buf, BUF_SIZE, WAVELET_16)
//    Wavelet.invTransform(buf, BUF_SIZE, WAVELET_4 )

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

    image(imgOut /* cam */, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
  }
}
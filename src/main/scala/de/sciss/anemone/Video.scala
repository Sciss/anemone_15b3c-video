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
  private[this] val VIDEO_WIDTH   = 1920
  private[this] val VIDEO_HEIGHT  = 1080
  private[this] val VIDEO_FPS     = 30
  private[this] val VIDEO_DEVICE  = "/dev/video1"
  private[this] val VIDEO_SIZE    = VIDEO_WIDTH * VIDEO_HEIGHT

  private[this] val WINDOW_WIDTH  = 1024 // VIDEO_WIDTH  / 2
  private[this] val WINDOW_HEIGHT =  768 // VIDEO_HEIGHT / 2
  private[this] val WINDOW_SIZE   = WINDOW_WIDTH * WINDOW_HEIGHT

  private[this] var cam: Capture = _
  private[this] val buf     = new Array[Float](WINDOW_WIDTH * WINDOW_HEIGHT)
  private[this] val imgOut  = new PImage(WINDOW_WIDTH, WINDOW_HEIGHT, PConstants.RGB /* ARGB */)

  private[this] val X_START = (VIDEO_WIDTH  - WINDOW_WIDTH) / 2    // XXX TODO -- parametrise
  private[this] val Y_START = (VIDEO_HEIGHT - WINDOW_WIDTH) / 2    // XXX TODO -- parametrise

  override def init(): Unit = {
    super.init()
    setPreferredSize(new Dimension(WINDOW_WIDTH, WINDOW_HEIGHT))
  }

  override def setup(): Unit = {
    size(WINDOW_WIDTH, WINDOW_HEIGHT)
    cam = new Capture(this, VIDEO_WIDTH, VIDEO_HEIGHT, VIDEO_DEVICE, VIDEO_FPS)
    cam.start()
    noLoop()
  }

  def captureEvent(c: Capture): Unit = {
    c.read()
    val pixIn   = c.pixels
    val pixOut  = imgOut.pixels
    var i       = Y_START * VIDEO_WIDTH + X_START
    var j       = 0
    while (j < WINDOW_SIZE) {
      val k = i + WINDOW_WIDTH
      while (i < k) {
        val rgbIn = pixIn(i)
        // pix(i) = pix(i) ^ 0xFFFFFF
        val red   = (rgbIn & 0xFF0000) >> 16
        val green = (rgbIn & 0x00FF00) >>  8
        val blue  =  rgbIn & 0x0000FF

        val bright = 0.299f * red + 0.587f * green + 0.114f * blue
        buf(j) = bright
        // ... TODO
        val bi = (bright * 0xFF).toInt
        val rgbOut = /* 0xFF000000 | */ (bi << 16) | (bi << 8) | bi
        pixOut(j) = rgbOut

        i += 1
        j += 1
      }
      i += VIDEO_WIDTH - WINDOW_WIDTH // next scan
    }

    imgOut.updatePixels()
    redraw()
  }

  override def draw(): Unit = {
    // if (cam.available()) cam.read()

    image(imgOut /* cam */, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
  }
}
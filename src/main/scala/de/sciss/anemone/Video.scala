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

import processing.core.PApplet
import processing.video.Capture

object Video extends Runnable {
  def main(args: Array[String]) = EventQueue.invokeAndWait(this)

  val VIDEO_WIDTH   = 1920
  val VIDEO_HEIGHT  = 1080
  val VIDEO_FPS     = 30
  val VIDEO_DEVICE  = "/dev/video1"

  val WINDOW_WIDTH  = VIDEO_WIDTH  / 2
  val WINDOW_HEIGHT = VIDEO_HEIGHT / 2

  def run(): Unit = {
    val sketch = new Video
    val frame = new javax.swing.JFrame("Anemone")
    frame.getContentPane.add(sketch)
    sketch.init()
    sketch.setPreferredSize(new Dimension(WINDOW_WIDTH, WINDOW_HEIGHT))
    frame.setResizable(false)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.pack()
    frame.setVisible(true)
  }
}
class Video extends PApplet {
  import Video._

  var cam: Capture = _

  override def setup(): Unit = {
    size(WINDOW_WIDTH, WINDOW_HEIGHT)
    cam = new Capture(this, VIDEO_WIDTH, VIDEO_HEIGHT, VIDEO_DEVICE, VIDEO_FPS)
    cam.start()
    noLoop()
  }

  def captureEvent(c: Capture): Unit = {
    c.read()
    redraw()
  }

  override def draw(): Unit = {
    // if (cam.available()) cam.read()

    image(cam, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
  }
}
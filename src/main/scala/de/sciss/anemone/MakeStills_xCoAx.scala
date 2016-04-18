/*
 *  MakeStills_xCoAx.scala
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

import java.awt.{BorderLayout, EventQueue}
import javax.swing.{JPanel, SpinnerNumberModel}

import de.sciss.file._
import de.sciss.numbers
import de.sciss.swingplus.Implicits._
import de.sciss.swingplus.{CloseOperation, Spinner}

import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Button, CheckBox, Component, FlowPanel, Frame, Label, Slider}

object MakeStills_xCoAx extends App with Runnable {
  EventQueue.invokeLater(this)

  def run(): Unit = {
    val base    = userHome / "Documents" / "projects" / "xCoAx2016" / "anemone"
    val photos  = base / "photos"
    val p1      = photos / "test3.jpg" // DSC01627m.jpg"

    val cfg = Video.Config(
      device = Video.DEVICE_NONE,
      deviceWidth = 1920, deviceHeight = 1080,
      screenWidth = 1024, screenHeight = 768
    )
    val sketch = new Video(cfg)

    val ggAnim = new CheckBox("Animate") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          if (selected) {
            sketch.advance = true
            sketch.loop()
          } else {
            sketch.noLoop()
          }
      }
    }

    val ggStep = Button("Step") {
      sketch.advance = true
      sketch.redraw()
    }

    val mRun    = new SpinnerNumberModel(1.0, 1.0, sketch.bufSize.toDouble, 0.1)
    val mAlgo1  = new SpinnerNumberModel(0, 0, sketch.numAlgorithms, 1)
    val mAlgo2  = new SpinnerNumberModel(0, 0, sketch.numAlgorithms, 1)

    var slidMute = false

    val ggRunSlid = new Slider {
      value = 0
      min   = 0
      max   = 200

      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          if (!slidMute) {
            import numbers.Implicits._
            val v = value.linexp(min, max, 1.0, sketch.bufSize)
            mRun.setValue(v)
          }
      }
    }

    def triggerRedraw(): Unit = {
      sketch.noLoop()
      sketch.advance = false
      sketch.redraw()
    }

    val ggRun = new Spinner(mRun) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          sketch.currentRun = mRun.getNumber.doubleValue()
          triggerRedraw()
          import numbers.Implicits._
          slidMute = true
          try {
            ggRunSlid.value = (sketch.currentRun.explin(1.0, sketch.bufSize, ggRunSlid.min, ggRunSlid.max) + 0.5).toInt
          } finally {
            slidMute = false
          }
      }
    }

    val ggAlgo1 = new Spinner(mAlgo1) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          sketch.previousAlgorithm = mAlgo1.getNumber.intValue()
          triggerRedraw()
      }
    }

    val ggAlgo2 = new Spinner(mAlgo2) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          sketch.fadingAlgorithm = mAlgo2.getNumber.intValue()
          triggerRedraw()
      }
    }

    val ggSave = Button("Save") {
      val name = s"out_${sketch.previousAlgorithm}_${sketch.fadingAlgorithm}_${sketch.currentRun.toInt}.png"
      val f    = base / "image_out" / name
      sketch.save(f.path)
    }

    val pCtrl = new FlowPanel(ggAnim, ggStep, new Label("Run:"), ggRun, ggRunSlid,
      new Label("A1:"), ggAlgo1, new Label("A2:"), ggAlgo2, ggSave)

    val panel = new JPanel(new BorderLayout) {
      add(sketch, BorderLayout.CENTER)
      add(pCtrl.peer, BorderLayout.SOUTH) // BorderPanel.Position.South)
    }

    val frame = new Frame {
      title = "Anemone"
      contents = Component.wrap(panel)
      this.defaultCloseOperation = CloseOperation.Exit
    }
    sketch.frame = frame.peer

    sketch.init()

    val i1      = sketch.loadImage(p1.path)
    sketch.camImage.copy(i1, 0, 0, i1.width, i1.height, 0, 0, sketch.camImage.width, sketch.camImage.height)

    frame.pack()
    frame.open()
  }
}
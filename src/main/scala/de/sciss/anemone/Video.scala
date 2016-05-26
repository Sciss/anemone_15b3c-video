/*
 *  Video.scala
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

import java.awt.event.KeyEvent
import java.awt.{Dimension, EventQueue}
import java.text.SimpleDateFormat
import javax.swing.WindowConstants

import de.sciss.file._
import de.sciss.fscape.spect.Wavelet
import de.sciss.kollflitz.impl.Urn
import de.sciss.numbers
import org.bytedeco.javacv.{FFmpegFrameGrabber, Java2DFrameConverter}
import processing.core.{PApplet, PConstants, PGraphicsJava2D, PImage}
import processing.video.Capture
import processing.{event => pe}

import scala.annotation.switch

object Video {
  val DEVICE_NONE = "none"

  case class Config(device: String = "/dev/video0", listDevices: Boolean = false,
                    deviceWidth: Int = 1920, deviceHeight: Int = 1080, fps: Int = 24,
                    screenWidth: Int = 1024, screenHeight: Int = 768,
                    x1: Int = -1, y1: Int = -1, w1: Int = -1, h1: Int = -1,
                    x2: Int = -1, y2: Int = -1, w2: Int = -1, h2: Int = -1,
                    fadeDur: Double = 60, noise: Double = 0.1, wavelet: Int = 8,
                    intervalDur: Double = 3 * 60 + 10, totalDur: Double = 15 * 60 + 10,
                    seed: Long = 0L /* -1L */, logging: Boolean = false, fast: Boolean = false)

  def main(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("anemone_15b3c-video") {
      opt[Unit  ]('l', "list"  ) text "list video capture devices" action { case (_, c) => c.copy(listDevices = true) }
      opt[String]('d', "device") text "video capture device" action { case (v, c) => c.copy(device = v) }
      opt[Int   ]('r', "fps"   ) text "frames per second" action { (v, c) => c.copy(fps = v) }
      opt[Int   ]('w', "device-width" ) text "video capture device width"  action { case (v, c) => c.copy(deviceWidth  = v) }
      opt[Int   ]('h', "device-height") text "video capture device height" action { case (v, c) => c.copy(deviceHeight = v) }
      opt[Int   ]('W', "screen-width" ) text "output screen width"  action { case (v, c) => c.copy(screenWidth  = v) }
      opt[Int   ]('H', "screen-height") text "output screen height" action { case (v, c) => c.copy(screenHeight = v) }
      opt[Int   ]("x1") text "first  frame left   (-1 = auto)" action { case (v, c) => c.copy(x1 = v) }
      opt[Int   ]("y1") text "first  frame top    (-1 = auto)" action { case (v, c) => c.copy(y1 = v) }
      opt[Int   ]("w1") text "first  frame width  (-1 = auto)" action { case (v, c) => c.copy(w1 = v) }
      opt[Int   ]("h1") text "first  frame height (-1 = auto)" action { case (v, c) => c.copy(h1 = v) }
      opt[Int   ]("x2") text "second frame left   (-1 = auto)" action { case (v, c) => c.copy(x2 = v) }
      opt[Int   ]("y2") text "second frame top    (-1 = auto)" action { case (v, c) => c.copy(y2 = v) }
      opt[Int   ]("w2") text "second frame width  (-1 = auto)" action { case (v, c) => c.copy(w2 = v) }
      opt[Int   ]("h2") text "second frame height (-1 = auto)" action { case (v, c) => c.copy(h2 = v) }
      opt[Double]("fade"   ) text "fade duration in seconds"   action { case (v, c) => c.copy(fadeDur = v) }
      opt[Double]("noise"  ) text "amount of noise (0 to 1)"   action { case (v, c) => c.copy(noise   = v) }
      opt[Int   ]("wavelet") text "wavelet coefficient set (4, 8, or 16)" validate { i =>
        if (i == 4 || i == 8 || i == 16) Right(()) else Left(s"Value $i is not one of 4, 8, 16")
      } action { case (v, c) => c.copy(h2 = v) }
      opt[Double]('i', "interval") text "interval between algorithms in seconds" action {
        case (v, c) => c.copy(intervalDur = v) }
      opt[Double]('t', "total") text "total duration for algorithm iteration in seconds" action {
        case (v, c) => c.copy(totalDur = v) }
      opt[Long  ]("seed") text "RNG seed for algorithm selection (-1 = auto)" action { case (v, c) => c.copy(seed = v) }
      opt[Unit  ]('v', "log"  ) text "enable debug logging" action { case (_, c) => c.copy(logging = true) }
      opt[Unit  ]('f', "fast" ) text "enable fast refresh" action { case (_, c) => c.copy(fast = true) }
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
    frame.setUndecorated(true)
    frame.setResizable(false)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    sketch.frame = frame
    sketch.init()
    frame.pack()
    frame.setVisible(true)
  }
}
final class Video(config: Video.Config) extends PApplet {
  import numbers.Implicits._

  private[this] val VIDEO_WIDTH   = config.deviceWidth  // 1920
  private[this] val VIDEO_HEIGHT  = config.deviceHeight // 1080
  private[this] val VIDEO_FPS     = config.fps // 24 // 30
  private[this] val VIDEO_DEVICE  = config.device
  // private[this] val VIDEO_SIZE    = VIDEO_WIDTH * VIDEO_HEIGHT

  private[this] val WINDOW_WIDTH  = math.min(VIDEO_WIDTH , config.screenWidth ) // VIDEO_WIDTH  / 2
  private[this] val WINDOW_HEIGHT = math.min(VIDEO_HEIGHT, config.screenHeight) // VIDEO_HEIGHT / 2
  private[this] val WINDOW_SIZE   = WINDOW_WIDTH * WINDOW_HEIGHT
  private[this] val _BUF_SIZE     = WINDOW_SIZE.nextPowerOfTwo

  def bufSize: Int = _BUF_SIZE

  private[this] val VIDEO_FIT_SCALE = {
    val hScale  = WINDOW_WIDTH .toFloat / VIDEO_WIDTH
    val vScale  = WINDOW_HEIGHT.toFloat / VIDEO_HEIGHT
    math.min(hScale, vScale)
  }

  private[this] val VIDEO_FIT_W = VIDEO_WIDTH  * VIDEO_FIT_SCALE
  private[this] val VIDEO_FIT_H = VIDEO_HEIGHT * VIDEO_FIT_SCALE

  // private[this] val SCAN  = VIDEO_WIDTH - WINDOW_WIDTH

  private[this] var _camImage : PImage = _
  private[this] var cam       : Capture = _
  private[this] var grabRay   : FFmpegFrameGrabber= _
  private[this] var grabBleed : FFmpegFrameGrabber= _

  def camImage: PImage = _camImage

  private[this] val buf1    = new Array[Float](_BUF_SIZE)
  private[this] val buf2    = new Array[Float](_BUF_SIZE)
  // private[this] val imgTmp  = new PImage(WINDOW_WIDTH, WINDOW_HEIGHT, PConstants.RGB /* ARGB */)
  private[this] val imgOut  = new PImage(WINDOW_WIDTH, WINDOW_HEIGHT, PConstants.RGB /* ARGB */)

  private[this] var X1 = if (config.x1 < 0) 0 else config.x1
  private[this] var Y1 = if (config.y1 < 0) (VIDEO_HEIGHT - WINDOW_HEIGHT) / 2 else config.y1
  private[this] var W1 = if (config.w1 < 0) WINDOW_WIDTH  else config.w1
  private[this] var H1 = if (config.h1 < 0) WINDOW_HEIGHT else config.h1
  private[this] var X2 = if (config.x2 < 0) VIDEO_WIDTH  - WINDOW_WIDTH else config.x2
  private[this] var Y2 = if (config.y2 < 0) (VIDEO_HEIGHT - WINDOW_HEIGHT) / 2 else config.y2
  private[this] var W2 = if (config.w2 < 0) WINDOW_WIDTH  else config.w2
  private[this] var H2 = if (config.h2 < 0) WINDOW_HEIGHT else config.h2

  private[this] val WAVELET         = Wavelet.getCoeffs(config.wavelet match {
    case  4 => Wavelet.COEFFS_DAUB4
    case  8 => Wavelet.COEFFS_DAUB8
    case 16 => Wavelet.COEFFS_DAUB16
  })
  
  private sealed trait RenderMode {
    def usesCam: Boolean
    def start(): Unit
    def stop(): Unit
  }
  private case object RenderAdjust extends RenderMode {
    val usesCam = true

    def start() = ()
    def stop () = ()
  }
  private case object RenderCam extends RenderMode {
    val usesCam = true
    def start() = ()
    def stop () = ()
  }
  private case object RenderBleed extends RenderMode {
    val usesCam = false
    def start(): Unit = grabBleed.start()
    def stop (): Unit = grabBleed.stop()
  }
  private case object RenderRay extends RenderMode {
    val usesCam = false
    def start(): Unit = grabRay.start()
    def stop (): Unit = grabRay.stop()
  }

  private[this] var renderMode      = RenderBleed /* RenderCam */: RenderMode

  private[this] var adjustCorner    = 0

  // private[this] var NORMALIZE = false
  private[this] val NUM_ALGORITHMS  = 5 // 6
  private[this] var ALGORITHM       = NUM_ALGORITHMS  // aka black

  private[this] var NOISE           = config.noise.toFloat // 0.1f

  private[this] val noiseBuf        = new Array[Float](0x20000)

  private[this] val FADE_DUR        = config.fadeDur // 60.0
  private[this] val FADE_FACTOR     = _BUF_SIZE.pow(1.0 / (FADE_DUR * VIDEO_FPS))
  // private[this] val FADE_STEP     = BUF_SIZE / (FADE_DUR * VIDEO_FPS)

  private[this] var stopTime        = 0L
  private[this] var nextEvent       = Long.MaxValue
  
  def numAlgorithms: Int = NUM_ALGORITHMS

  private def clipFrames(): Unit = {
    W1  = W1.clip(8, WINDOW_WIDTH )
    H1  = H1.clip(8, WINDOW_HEIGHT)
    W2  = W2.clip(8, WINDOW_WIDTH )
    H2  = H2.clip(8, WINDOW_HEIGHT)

    X1  = X1.clip(0, VIDEO_WIDTH  - W1)
    Y1  = Y1.clip(0, VIDEO_HEIGHT - H1)
    X2  = X2.clip(0, VIDEO_WIDTH  - W2)
    Y2  = Y2.clip(0, VIDEO_HEIGHT - H2)
  }

  override def init(): Unit = {
    super.init()
    updateNoise()
    setPreferredSize(new Dimension(WINDOW_WIDTH, WINDOW_HEIGHT))
    // setRenderMode(RenderCam)
    clipFrames()
  }

  private def setRenderMode(m: RenderMode): Unit = {
    val old = renderMode
    renderMode = m
    val hasCursor = renderMode == RenderAdjust || (renderMode == RenderCam && cam == null)
    if (hasCursor) cursor() else noCursor()
    if (old != m) {
      println(s"Mode: $m")
      old.stop()
      val stopCam   =  old.usesCam && !m.usesCam
      val startCam  = !old.usesCam &&  m.usesCam
      if (stopCam  && cam != null) cam.stop ()
      if (startCam && cam != null) cam.start()
      m.start()
    }
  }

  private def updateNoise(): Unit = {
    var i = 0
    while (i < noiseBuf.length) {
      noiseBuf(i) = (java.lang.Math.random().toFloat - 0.5f) * NOISE
      i += 1
    }
  }

  private[this] val java2dFrameConv = new Java2DFrameConverter

  override def setup(): Unit = {
    size(WINDOW_WIDTH, WINDOW_HEIGHT)
    if (VIDEO_DEVICE == Video.DEVICE_NONE) {
      _camImage = new PImage(VIDEO_WIDTH, VIDEO_HEIGHT)
      noLoop()
    } else {
      cam = new Capture(this, VIDEO_WIDTH, VIDEO_HEIGHT, VIDEO_DEVICE, VIDEO_FPS)
      _camImage = cam
      // cam.start()
    }
    val fVideoRay   = userHome / "Documents" / "projects" / "Langzeitbelichtung" / "material" / "convolve.mp4"
    grabRay         = new FFmpegFrameGrabber(fVideoRay.path)
    val fVideoBleed = userHome / "Documents" / "projects" / "Anemone" / "minuten" / "out3.mp4"
    grabBleed       = new FFmpegFrameGrabber(fVideoBleed.path)

    // noLoop()
    imgOut.loadPixels()
    setRenderMode(RenderCam)
  }

  private[this] var bufShift = 0

  private def crop(x: Int, y: Int, w: Int, h: Int, buf: Array[Float]): Unit = {
    imgOut.copy(_camImage, x, y, w, h, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
    imgOut.loadPixels()
    val pixIn = imgOut.pixels

    // ---- copy gray-scale image to buffer ----
    var j   = 0
    var k   = bufShift
    val ws  = WINDOW_SIZE
    val bs  = _BUF_SIZE
    while (j < ws) {
      val rgbIn = pixIn(j)
      // pix(i) = pix(i) ^ 0xFFFFFF
      val red   = (rgbIn & 0xFF0000) >> 16
      val green = (rgbIn & 0x00FF00) >>  8
      val blue  =  rgbIn & 0x0000FF

      val bright = (0.299f * red + 0.587f * green + 0.114f * blue) / 0xFF
      // val bright = if (bright0 > 1f) 1f else bright0
      buf(k) = bright
      j += 1
      k = (k + 1) % bs
    }
    // println(s"aqui j = $j, k = $k")
    while (j < bs) {
      buf(k) = 0f
      j += 1
      k = (k + 1) % bs
    }
  }

  // N.B.: This seems to happen _outside_ of AWT event loop
  def captureEvent(c: Capture): Unit = if (!config.fast) {
    c.read()
    redraw()
  }

//  private def renderAdjustment(): Unit = {
//    imgOut.copy(cam, 0, 0, VIDEO_WIDTH, VIDEO_HEIGHT, 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
//  }

  private[this] var _previousAlgorithm = ALGORITHM
  private[this] var _fadingAlgorithm   = ALGORITHM
  
  def previousAlgorithm: Int = _previousAlgorithm
  def previousAlgorithm_=(value: Int): Unit = _previousAlgorithm = value

  def fadingAlgorithm: Int = _fadingAlgorithm
  def fadingAlgorithm_=(value: Int): Unit = _fadingAlgorithm = value

  private[this] val algorithmUrn =
    new Urn[Int](0 until NUM_ALGORITHMS, infinite = true)(
      new util.Random(if (config.seed == -1L) System.currentTimeMillis() else config.seed))

  private def log(what: => String): Unit = if (config.logging) println(what)

  private def checkNextEvent(): Unit =
    if (!isFading && ALGORITHM != _fadingAlgorithm) {
      _previousAlgorithm = _fadingAlgorithm
      _fadingAlgorithm   = ALGORITHM
      _currentRun        = 1.0
      log(s"Next fade from ${_previousAlgorithm} to ${_fadingAlgorithm}")
    }

  private def setAlgorithm(id: Int): Unit = {
    ALGORITHM = id
    log(s"setAlgorithm($id)")
    checkNextEvent()
  }

  private def runAlgorithm(id: Int, start: Int, stop: Int): Unit = {
    var i = start
    (id: @switch) match {
      case 0 =>
        while (i < stop) {
          buf1(i) = math.max(buf1(i), buf2(i))
          // buf1(i) = math.max(buf1(i), buf2(i)) - math.min(buf1(i), buf2(i))
          i += 1
        }
      case 1 =>
        while (i < stop) {
          buf1(i) = buf1(i).toInt ^ buf2(i).toInt
          i += 1
        }
      //      case 2 =>
      //        while (i < BUF_SIZE) {
      //          buf1(i) = (buf1(i) atan2 buf2(i)) * 0.25f
      //          i += 1
      //        }
      //      case 3 =>
      //        while (i < BUF_SIZE) {
      //          buf1(i) = buf1(i) hypotx buf2(i)  // !
      //          i += 1
      //        }
      case 2 =>
        while (i < stop) {
          if (i % 2 == 0) buf1(i) = buf2(i)
          i += 1
        }
      case 3 =>
        while (i < stop) {
          buf1(i) = math.max(buf1(i), buf2(i)) - math.min(buf1(i), buf2(i))
          i += 1
        }
      case 4 =>
        var even = true
        var k = (i + 1).nextPowerOfTwo
        while (i < stop) {
          if (even) buf1(i) = buf2(i)
          i += 1
          if (i == k) {
            k <<= 1
            even = !even
          }
        }
      case _ =>
        while (i < stop) {
          buf1(i) = 0f
          i += 1
        }
    }
  }

  private def startEvent(): Unit = {
    val now   = System.currentTimeMillis()
    stopTime  = now + (config.totalDur * 1000).toLong
    nextEvent = now
    log(s"startEvent - now = ${formatTime(now)}, stopTime = ${formatTime(stopTime)}")
    mkNextEvent()
  }

  private[this] val timeFormat = new SimpleDateFormat("hh:mm:ss.SSS")

  private def formatTime(n: Long): String = timeFormat.format(new java.util.Date(n))

  private def mkNextEvent(): Unit = {
    // val now   = System.currentTimeMillis()
    nextEvent = nextEvent + (config.intervalDur * 1000).toLong
    log(s"mkNextEvent - ${formatTime(nextEvent)}")
    if (nextEvent >= stopTime) stopEvent()
    setAlgorithm(algorithmUrn.next())
  }

  private def stopEvent(): Unit = {
    log("stopEvent")
    nextEvent = Long.MaxValue
  }

  private def isStopped: Boolean =
    nextEvent == Long.MaxValue

  private[this] var _currentRun: Double = _BUF_SIZE // 1.0

  def currentRun: Double = _currentRun
  def currentRun_=(value: Double): Unit = _currentRun = value

  private def isFading: Boolean = _currentRun < _BUF_SIZE

  private[this] var ADVANCE = true

  def advance: Boolean = ADVANCE
  def advance_=(value: Boolean): Unit = ADVANCE = value

  private def renderCamFrame(): Unit = {
    crop(x = X1, y = Y1, w = W1, h = H1, buf = buf1)
    crop(x = X2, y = Y2, w = W2, h = H2, buf = buf2)

    val bs = _BUF_SIZE
    Wavelet.fwdTransform(buf1, bs, WAVELET)
    Wavelet.fwdTransform(buf2, bs, WAVELET)

    if (isFading) {
      val cr = _currentRun.toInt - 1
      runAlgorithm(id = _previousAlgorithm, start = cr, stop = bs)
      runAlgorithm(id = _fadingAlgorithm  , start = 0 , stop = cr)
      if (ADVANCE) {
        _currentRun *= FADE_FACTOR
        checkNextEvent()
      }
    } else {
      runAlgorithm(id = _fadingAlgorithm, start = 0, stop = bs)
    }

    // var MIN = Float.PositiveInfinity
    // var MAX = Float.NegativeInfinity
    // val gain = (1.0f / BUF_SIZE).sqrt
    // println(s"MIN = $MIN, MAX = $MAX")

    Wavelet.invTransform(buf1, bs, WAVELET)

//    // ---- normalize ----
//    if (NORMALIZE) {
//      i = 1
//      var MIN = buf1(0)
//      var MAX = buf1(0)
//      while (i < WINDOW_SIZE) {
//        val x = buf1(i)
//        if (x < MIN) MIN = x
//        if (x > MAX) MAX = x
//        i += 1
//      }
//      // println(s"MIN = $MIN, MAX = $MAX")
//      if (MIN < MAX) {
//        i = 0
//        val off   = -MIN
//        val scale = 1.0f / (MAX - MIN)
//        while (i < WINDOW_SIZE) {
//          buf1(i) = (buf1(i) + off) * scale
//          i += 1
//        }
//      }
//    }

    if (NOISE > 0f) {
      var i = 1
      var j = (java.lang.Math.random() * 0x20000).toInt
      var k = bufShift
      val ws = WINDOW_SIZE
      val bs = _BUF_SIZE
      while (i < ws) {
        j = (j + 1) % 0x20000
        buf1(k) += noiseBuf(j)
        i += 1
        k = (k + 1) % bs
      }
    }

    // ---- copy buffer to p-image ----
    imgOut.loadPixels()
    val pixOut  = imgOut.pixels
    var j       = 0
    var k       = bufShift
    val ws      = WINDOW_SIZE
    while (j < ws) {
      val b0      = buf1(k)
      val b1      = if (b0 < 0f) 0f else b0
      val bright  = if (b1 > 1f) 1f else b1
      val bi      = (bright * 0xFF).toInt
      val rgbOut  = 0xFF000000 | (bi << 16) | (bi << 8) | bi
      pixOut(j)   = rgbOut
      j += 1
      k = (k + 1) % bs
    }
    imgOut.updatePixels()

    bufShift = (bufShift + 1) % bs

    val now = System.currentTimeMillis()
    if (now >= nextEvent) mkNextEvent()
  }

  override def draw(): Unit = {
    if (renderMode.usesCam && config.fast && cam.available()) cam.read()

    val w = getWidth
    val h = getHeight
    fill(0f)
    noStroke()
    rect(0, 0, w, h)
    
    renderMode match {
      case RenderBleed =>
        drawBleed()
      case RenderRay =>
        drawRay()
      case RenderCam =>
        renderCamFrame()
        drawCam()
      case RenderAdjust => 
        drawAdjustment()
    }
  }

  private def red  (): Unit = stroke(0xFF, 0x00, 0x00)
  private def green(): Unit = stroke(0x00, 0xC0, 0x00)

  private def drawRay  (): Unit = drawFFmpeg(grabRay  )
  private def drawBleed(): Unit = drawFFmpeg(grabBleed)

  private def drawFFmpeg(grab: FFmpegFrameGrabber): Unit = {
    val frame = grab.grab()
    val img   = java2dFrameConv.getBufferedImage(frame, 1.0)
    // val imgP  = new PImage(img)
    val w     = getWidth
    val h     = getHeight
    val wi    = img.getWidth()
    val hi    = img.getHeight()
    val x = (w - wi ) >> 1
    val y = (h - hi ) >> 1

    val g2 = this.g.asInstanceOf[PGraphicsJava2D].g2
    g2.drawImage(img, x, y, this)
    // image(imgP /* imgOut */, x, y)
  }

  private def drawCam(): Unit = {
    val w = getWidth
    val h = getHeight
    val x = (w - WINDOW_WIDTH ) >> 1
    val y = (h - WINDOW_HEIGHT) >> 1
    image(imgOut, x, y)
  }

  private def drawAdjustment(): Unit = {
    val w = getWidth
    val h = getHeight
    val x = (w - VIDEO_FIT_W) / 2
    val y = (h - VIDEO_FIT_H) / 2
    image(_camImage, x, y, VIDEO_FIT_W, VIDEO_FIT_H)

    translate(x, y)
    scale(VIDEO_FIT_SCALE)
    noFill()

    if (adjustCorner == 0) green() else red()
    line(X1, Y1, X1 + W1 - 1, Y1)
    line(X1, Y1, X1, Y1 + H1 - 1)
    if (adjustCorner == 1) green() else red()
    line(X1, Y1 + H1 - 1, X1 + W1 - 1, Y1 + H1 - 1)
    line(X1 + W1 - 1, Y1, X1 + W1 - 1, Y1 + H1 - 1)
    if (adjustCorner == 2) green() else red()
    line(X2, Y2, X2 + W2 - 1, Y2)
    line(X2, Y2, X2, Y2 + H2 - 1)
    if (adjustCorner == 3) green() else red()
    line(X2, Y2 + H2 - 1, X2 + W2 - 1, Y2 + H2 - 1)
    line(X2 + W2 - 1, Y2, X2 + W2 - 1, Y2 + H2 - 1)
  }

  override def keyPressed(e: pe.KeyEvent): Unit = {
    if (key == PConstants.ESC) key = 0    // avoid default behaviour of quitting the system
    renderMode match {
      case RenderAdjust => keyPressedAdjust(e)
      case RenderCam    => keyPressedCam   (e)
      case RenderRay    => keyPressedRay   (e)
      case RenderBleed  => keyPressedBleed (e)
    }
  }

  private def keyPressedRay(e: pe.KeyEvent): Unit = {
    handleStdKeys(e)
    e.getKeyCode match {
      case KeyEvent.VK_ENTER => setRenderMode(RenderBleed)
      case KeyEvent.VK_LEFT  =>
        setRenderMode(RenderCam)
        startEvent()
      case _ =>
    }
  }

  private def keyPressedBleed(e: pe.KeyEvent): Unit = {
    handleStdKeys(e)
    e.getKeyCode match {
      case KeyEvent.VK_ENTER => // (End)
      case KeyEvent.VK_LEFT  =>
        setRenderMode(RenderRay)
      case _ =>
    }
  }

  private def keyPressedAdjust(e: pe.KeyEvent): Unit = {
    val amt = if (e.isShiftDown) 4 else 1
    e.getKeyCode match {
      case KeyEvent.VK_A => // exit adjustment mode
        println(s"--x1 $X1 --y1 $Y1 --w1 $W1 --h1 $H1 --x2 $X2 --y2 $Y2 --w2 $W2 --h2 $H2")
        setRenderMode(RenderCam)

      case KeyEvent.VK_SPACE => adjustCorner = (adjustCorner + 1) % 4
      case KeyEvent.VK_LEFT  =>
        adjustCorner match {
          case 0 => X1 -= amt // first  left/top
          case 1 => W1 -= amt // first  bottom/right
          case 2 => X2 -= amt // second left/top
          case 3 => W2 -= amt // second bottom/right
        }

      case KeyEvent.VK_RIGHT =>
        adjustCorner match {
          case 0 => X1 += amt // first  left/top
          case 1 => W1 += amt // first  bottom/right
          case 2 => X2 += amt // second left/top
          case 3 => W2 += amt // second bottom/right
        }

      case KeyEvent.VK_UP =>
        adjustCorner match {
          case 0 => Y1 -= amt // first  left/top
          case 1 => H1 -= amt // first  bottom/right
          case 2 => Y2 -= amt // second left/top
          case 3 => H2 -= amt // second bottom/right
        }

      case KeyEvent.VK_DOWN  =>
        adjustCorner match {
          case 0 => Y1 += amt // first  left/top
          case 1 => H1 += amt // first  bottom/right
          case 2 => Y2 += amt // second left/top
          case 3 => H2 += amt // second bottom/right
        }

      case _ =>
    }
    clipFrames()
  }

  private def resetMode(): Unit = {
    _previousAlgorithm  = NUM_ALGORITHMS
    _fadingAlgorithm    = NUM_ALGORITHMS
    ALGORITHM           = NUM_ALGORITHMS
    _currentRun         = _BUF_SIZE
    nextEvent           = Long.MaxValue
    setRenderMode(RenderCam)
  }

  private def handleStdKeys(e: pe.KeyEvent): Unit =
    if (e.isControlDown) {
      if (e.getKeyCode == KeyEvent.VK_F && e.isShiftDown) { // toggle full-screen
        if (frame != null) {
          val gc = frame.getGraphicsConfiguration
          val sd = gc.getDevice
          sd.setFullScreenWindow(if (sd.getFullScreenWindow == frame) null else frame)
        }
      } else if (e.getKeyCode == KeyEvent.VK_Q) {
        sys.exit()
      }
    } else {
      e.getKeyCode match {
        case KeyEvent.VK_A     => setRenderMode(RenderAdjust)  // enter adjustment mode
        //        case KeyEvent.VK_N     =>
        //          NORMALIZE = !NORMALIZE
        //          println(s"normalize = ${if (NORMALIZE) "on" else "off"}")
        case KeyEvent.VK_ESCAPE =>
          resetMode()
        case _ =>
      }
    }

  private def keyPressedCam(e: pe.KeyEvent): Unit = {
    handleStdKeys(e)
    if (!e.isControlDown) e.getKeyCode match {
      case KeyEvent.VK_A     => setRenderMode(RenderAdjust)  // enter adjustment mode
//        case KeyEvent.VK_N     =>
//          NORMALIZE = !NORMALIZE
//          println(s"normalize = ${if (NORMALIZE) "on" else "off"}")
//      case KeyEvent.VK_ESCAPE =>
//        resetMode()

      case KeyEvent.VK_ENTER =>
        if (isStopped) {
          startEvent()
        } else {
          stopEvent()
          setRenderMode(RenderRay)
        }

      case KeyEvent.VK_RIGHT =>
        setAlgorithm((ALGORITHM + 1) % NUM_ALGORITHMS)
        stopEvent()
        println(s"algorithm = $ALGORITHM")
      case KeyEvent.VK_LEFT  =>
        setAlgorithm((ALGORITHM - 1 + NUM_ALGORITHMS) % NUM_ALGORITHMS)
        stopEvent()
        println(s"algorithm = $ALGORITHM")
      case KeyEvent.VK_UP    =>
        NOISE = math.min(0.9f, NOISE + 0.1f)
        println(s"noise = $NOISE")
        updateNoise()
      case KeyEvent.VK_DOWN  =>
        NOISE = math.max(0.0f, NOISE - 0.1f)
        println(s"noise = $NOISE")
      case KeyEvent.VK_1 if NUM_ALGORITHMS >= 1 => setAlgorithm(1 - 1); stopEvent()
      case KeyEvent.VK_2 if NUM_ALGORITHMS >= 2 => setAlgorithm(2 - 1); stopEvent()
      case KeyEvent.VK_3 if NUM_ALGORITHMS >= 3 => setAlgorithm(3 - 1); stopEvent()
      case KeyEvent.VK_4 if NUM_ALGORITHMS >= 4 => setAlgorithm(4 - 1); stopEvent()
      case KeyEvent.VK_5 if NUM_ALGORITHMS >= 5 => setAlgorithm(5 - 1); stopEvent()
      case KeyEvent.VK_6 if NUM_ALGORITHMS >= 6 => setAlgorithm(6 - 1); stopEvent()
      case KeyEvent.VK_7 if NUM_ALGORITHMS >= 7 => setAlgorithm(7 - 1); stopEvent()
      case KeyEvent.VK_8 if NUM_ALGORITHMS >= 8 => setAlgorithm(8 - 1); stopEvent()
      case KeyEvent.VK_9 if NUM_ALGORITHMS >= 9 => setAlgorithm(9 - 1); stopEvent()
      case KeyEvent.VK_0                        => setAlgorithm(NUM_ALGORITHMS); stopEvent() // black
      case _ =>
    }
  }

  // def installFullScreenKey(frame: java.awt.Frame): Unit = this.frame = frame
}

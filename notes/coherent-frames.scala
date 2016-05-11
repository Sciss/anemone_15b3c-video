val base = userHome/"Documents"/"devel"/"anemone_15b3c-video"
val inDir = base/"conv_image_out"
val outDir = base/"test_image_out"

object fileOrd extends Ordering[File] {
  def compare(a: File, b: File): Int = {
    val (an, _) = a.baseAndExt
    val (bn, _) = b.baseAndExt
    if (an.startsWith("frame-") && bn.startsWith("frame-")) {
      an.substring(6).toInt compareTo bn.substring(6).toInt
    } else {
      an compareTo bn
    }
  }
}

val inFiles = inDir.children(_.ext == "png").sorted(fileOrd) // By(_.name)
if (!outDir.exists()) outDir.mkdir()
inFiles.zipWithIndex.foreach { case (inF, idx) =>
  val outF = outDir/s"frame-$idx.png"
  if (!outF.exists()) {
    import sys.process._
    Seq("ln", "-s", inF.path, outF.path).!
  }
}

// inFiles.foreach(f => println(f.name))
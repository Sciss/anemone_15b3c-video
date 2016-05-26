name               := "anemone_15b3c-video"
version            := "0.2.0-SNAPSHOT"
organization       := "de.sciss"
scalaVersion       := "2.11.8"
description        := "Video sketch for the performance of the eponymous piece"
homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses           := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

lazy val processingVersion  = "2.2.1" // "3.0b5" is bullocks, breaks API etc.
lazy val jnaVersion         = "3.4.0"
lazy val scissDSPVersion    = "1.2.2"
lazy val numbersVersion     = "0.1.1"
lazy val scoptVersion       = "3.4.0"
lazy val kollFlitzVersion   = "0.2.0"
lazy val swingPlusVersion   = "0.2.1"
lazy val fileUtilVersion    = "1.1.1"
lazy val audioFileVersion   = "1.4.5"
lazy val processorVersion   = "0.4.0"

libraryDependencies ++= Seq(
  "org.processing"   %  "video"           % processingVersion,   // exclude("net.java.dev.jna", "jna") - doesn't work
  "de.sciss"         %% "scissdsp"        % scissDSPVersion,
  "de.sciss"         %% "numbers"         % numbersVersion,
  "com.github.scopt" %% "scopt"           % scoptVersion,
  "de.sciss"         %% "kollflitz"       % kollFlitzVersion,
  "de.sciss"         %% "swingplus"       % swingPlusVersion,
  "de.sciss"         %% "fileutil"        % fileUtilVersion,
  "de.sciss"         %% "scalaaudiofile"  % audioFileVersion,
  "de.sciss"         %% "processor"       % processorVersion
)

javaCppPresetLibs ++= Seq(
  "ffmpeg" -> "2.8.1"
)

dependencyOverrides += "net.java.dev.jna" % "jna" % jnaVersion

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")


name               := "anemone_15b3c-video"

version            := "0.1.0-SNAPSHOT"

organization       := "de.sciss"

scalaVersion       := "2.11.7"

description        := "Video sketch for the performance of the eponymous piece"

homepage           := Some(url(s"https://github.com/Sciss/${name.value}"))

licenses           := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

lazy val processingVersion  = "2.2.1" // "3.0b5" is bullocks, breaks API etc.
lazy val gstreamerVersion   = "1.5"
lazy val jnaVersion         = "3.4.0"

libraryDependencies ++= Seq(
  "org.processing" % "video" % processingVersion,   // exclude("net.java.dev.jna", "jna") - doesn't work
  "com.googlecode.gstreamer-java" % "gstreamer-java" % gstreamerVersion
)

dependencyOverrides += "net.java.dev.jna" % "jna" % jnaVersion

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")


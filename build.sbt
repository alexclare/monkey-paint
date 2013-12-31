name := "MonkeyPaint"

version := "2.0.0"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.3"
)

fork in run := true

// TODO add jar generator/proguard/etc.

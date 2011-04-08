import sbt._

class MonkeyPaint(info: ProjectInfo) extends DefaultProject(info)
with ProguardProject {
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.0"
  
  override def fork = forkRun

  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
  override def proguardOptions = List(
    "-keep class MonkeyPaint$MonkeyApplet { *; }",
    "-keep class processing.core.PGraphics3D { *; }",
    proguardKeepMain("MonkeyPaint"))
}

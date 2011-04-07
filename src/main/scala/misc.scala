import processing.core._

case class RGBColor(Red: Int, Green: Int, Blue: Int)
object RGBColor {
  def apply(p: Int): RGBColor =
    RGBColor(p >> 16 & 0xFF, p >> 8 & 0xFF, p & 0xFF)

  implicit def toInt(c: RGBColor): Int =
    (c.Red << 16) + (c.Green << 8) + c.Blue

  def distance(a: RGBColor, b: RGBColor): Long = {
    val dr = a.Red - b.Red
    val dg = a.Green - b.Green
    val db = a.Blue - b.Blue
    dr*dr + dg*dg + db*db
  }
}

object Utility {
  def loadAndScale(image: String, width: String,
                   height: String)(implicit applet: PApplet): PImage = {
    val img = applet.loadImage(image)
    val (scaleWidth, scaleHeight) =
      scaleDims(img.width, img.height, optional(width), optional(height))
    img.resize(scaleWidth, scaleHeight)
    img
  }

  def scaleDims(actualWidth: Int, actualHeight: Int, scaledWidth: Option[Int],
                scaledHeight: Option[Int]): (Int, Int) =
    scaledWidth match {
      case Some(w) => scaledHeight match {
        case Some(h) => (w, h)
        case None    => (w, w*actualHeight/actualWidth)
      }
      case None => scaledHeight match {
        case Some(h) => (h*actualWidth/actualHeight, h)
        case None    => (actualWidth, actualHeight)
      }
  }

  def optional[A](str: String)(implicit cfn: String => A): Option[A] = {
    try {
      Some(cfn(str))
    } catch {
      case _ => None
    }
  }

  def orElse[A](str: String, default: A)(implicit cfn: String => A): A =
    optional(str).getOrElse(default)

  implicit def ToInt(s: String): Int = augmentString(s).toInt
  implicit def ToDouble(s: String): Double = augmentString(s).toDouble
}


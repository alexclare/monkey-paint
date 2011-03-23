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
  def scaleDims(actualWidth: Int, actualHeight: Int,
                scaledWidth: String, scaledHeight: String): (Int, Int) =
    scaledWidth match {
      case Int(w) => scaledHeight match {
        case Int(h) => (w, h)
        case _      => (w, w*actualHeight/actualWidth)
      }
      case _ => scaledHeight match {
        case Int(h) => (h*actualWidth/actualHeight, h)
        case _      => (actualWidth, actualHeight)
      }
  }
  
  def intOrElse(str: String, default: Int): Int = str match {
    case Int(i) => i
    case _ => default
  }
}

object Int {
  def unapply(str: String): Option[Int] = {
    try {
      Some(str.toInt)
    }
    catch {
      case ex: NumberFormatException => None
    }
  }
}

import scala.math
import scala.util.Random

//import processing.core._

case class Point(x: Int, y: Int)

/** The base Brush class ensures that the random-number generator is shared
 *    and that the points returned by each stroke are bounded by the
 *    dimensions of the image, ready to be fed into the pixel array.
 */
abstract class Brush (val rng: Random, dims: Point) {
  val width = dims.x
  val height = dims.y

  def stroke: (RGBColor, Set[Int])

  def stroke(color: RGBColor, points: Set[Point]) = {
    (color, points.filter {
      (p) => p.x >= 0 && p.y >= 0 && p.x < width && p.y < height
      }.map((p) => p.x + width*p.y))
  }
}

/** Performs a fixed (dx, dy) translation on the brush stroke
 */
trait Translate extends Brush {
  def dx: Int
  def dy: Int

  abstract override def stroke(color: RGBColor, points: Set[Point]) = {
    super.stroke(color, points.map((p) => Point(p.x + dx, p.y + dy)))
  }
}

/** Performs a random translation (up to the canvas bounds) on the stroke
 */
trait RandomTranslate extends Brush {
  abstract override def stroke(color: RGBColor, points: Set[Point]) = {
    val (dx, dy) = (rng.nextInt(width), rng.nextInt(height))
    super.stroke(color, points.map((p) => Point(p.x + dx, p.y + dy)))
  }
}

/** Performs a random rotation between a certain range (default 0-360 degrees)
 *    on the brush stroke
 *
 *  TODO: Implement vector graphics to remove blank patches due to rounding
 */
trait RandomRotate extends Brush {
  val rotationRange = (0, 2*math.Pi)

  abstract override def stroke(color: RGBColor, points: Set[Point]) = {
    val angle = rotationRange._1 +
      (rng.nextDouble*(rotationRange._2-rotationRange._1))
    val (cosa, sina) = (math.cos(angle), math.sin(angle))
    super.stroke(color, points.map {
      (p) => Point((p.x*cosa + p.y*sina).toInt,
                   (-p.x*sina + p.y*cosa).toInt)
    })
  }
}

/** Apply a random color to the brush stroke
 */
trait RandomColor extends Brush {
  abstract override def stroke(color: RGBColor, points: Set[Point]) = {
    super.stroke(RGBColor(rng.nextInt(255),
                          rng.nextInt(255),
                          rng.nextInt(255)), points)
  }
}

/* TODO: Clean up and implement in GUI - single color shade 
trait FixedShade extends Brush {
  abstract override def stroke(color: RGBColor, points: Set[Point]) = {
    val col = RGBColor(0, 255, 0)
    val shade = rng.nextDouble
    super.stroke(RGBColor((col.Red * shade).toInt,
                          (col.Green * shade).toInt,
                          (col.Blue * shade).toInt), points)
  }
}
*/

/** Base trait for brushes that use a fixed set of points for each stroke
 */
trait StaticBrush extends Brush {
  def points: Set[Point]
  def stroke = super.stroke(RGBColor(0, 0, 0), points)
}


class RandomWalk (r: Random, dims: Point, steps: Int,
                  thickness: Int) extends Brush(r, dims)
with RandomTranslate with RandomColor {
  def stroke = super.stroke(RGBColor(0, 0, 0), {
    import scala.collection.mutable.ArrayBuffer
    var dir = 0
    var pos = Point(0, 0)
    val points = ArrayBuffer.empty[Point]
    for (i <- 0 until steps) {
      for (j <- -thickness/2 to thickness/2) {
        if (dir == 0 || dir == 1) points.append(Point(pos.x, pos.y+j))
        else points.append(Point(pos.x+j, pos.y))
      }
      dir = rng.nextInt(4)
      pos = dir match {
        case 0 => Point(pos.x + 1, pos.y)
        case 1 => Point(pos.x - 1, pos.y)
        case 2 => Point(pos.x, pos.y + 1)
        case 3 => Point(pos.x, pos.y - 1)
      }
    }
    points.toSet
  })
}

class IsocelesBrush (r: Random, dims: Point,
                     base: Int, h: Int) extends Brush(r, dims)
with RandomTranslate with RandomRotate with Translate
with RandomColor with StaticBrush {
  val (dx, dy) = (0, -h/2)

  val slope = 2*h/base.toDouble
  val points = (for {
    y <- 1 to h
    x <- -(y/slope).toInt to (y/slope).toInt
  } yield Point(x,y)).toSet
}

class SquareBrush (r: Random, dims: Point, size: Int) extends Brush(r, dims)
with RandomTranslate with RandomRotate with RandomColor with StaticBrush {
  override val rotationRange = (0, math.Pi/2.0)
  val points = (for (x <- -size/2 to size/2; y <- -size/2 to size/2)
                yield Point(x, y)).toSet
}

/* TODO: clean up inverse/not and implement in GUI
class StampBrush (r: Random, dims: Point,
                  image: PImage, threshold: Double,
                  inverse: Boolean) extends Brush(r, dims)
with RandomTranslate with RandomRotate with Translate 
with RandomColor with StaticBrush {
  val (dx, dy) = (0, -image.height/2)
  val points = {
    val bw = new PImage(image.width, image.height)
    bw.copy(image, 0, 0, image.width, image.height,
            0, 0, image.width, image.height)
    bw.filter(PConstants.THRESHOLD, threshold.toFloat)
    bw.loadPixels
    (for {
      x <- 0 until image.width
      y <- 0 until image.height
      if (((bw.pixels(x + y*image.width) & 0xFF) > 0) ^ inverse)
    } yield Point(x,y)).toSet
  }
}
*/

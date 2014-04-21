package archery

import scala.math.{min, max, sqrt}

/**
 * Geometry represents a box (or point).
 * 
 * (x1, y1) is the lower left point and (x2, y2) is upper right. So
 * Box(1, 2, 5, 4) is at (1, 2) and is 4 wide and 2 high. Obviously
 * for points, (x1, y1) == (x2, y2) so both points are the same.
 */
sealed trait Geom {
  def x: Float
  def y: Float
  def x2: Float
  def y2: Float
  def width: Float = x2 - x
  def height: Float = y2 - y

  def area: Float = width * height

  import java.lang.Float.{isNaN, isInfinite}

  def distance(pt: Point): Double =
    sqrt(distanceSquared(pt))

  def distanceSquared(pt: Point): Double = {
    val dx = (if (pt.x < x) x - pt.x else if (pt.x < x2) 0F else pt.x - x2).toDouble
    val dy = (if (pt.y < y) y - pt.y else if (pt.y < y2) 0F else pt.y - y2).toDouble
    dx * dx + dy * dy
  }

  def isFinite: Boolean =
    !(isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y) ||
      isNaN(x2) || isInfinite(x2) ||
      isNaN(y2) || isInfinite(y2))

  def toBox: Box = Box(x, y, x2, y2)

  def lowerLeft: Point = Point(x, y)

  def upperRight: Point = Point(x2, y2)

  def contains(geom: Geom): Boolean =
    x <= geom.x && geom.x2 <= x2 && y <= geom.y && geom.y2 <= y2

  def intersects(geom: Geom): Boolean =
    x <= geom.x2 && geom.x <= x2 && y <= geom.y2 && geom.y <= y2

  def wraps(geom: Geom): Boolean =
    x < geom.x && geom.x2 < x2 && y < geom.y && geom.y2 < y2

  def expandArea(geom: Geom): Float = {
    val px1 = min(x, geom.x)
    val py1 = min(y, geom.y)
    val px2 = max(x2, geom.x2)
    val py2 = max(y2, geom.y2)
    val a = (py2 - py1) * (px2 - px1)
    a - area
  }

  def expand(geom: Geom): Box = {
    val px1 = min(x, geom.x)
    val py1 = min(y, geom.y)
    val px2 = max(x2, geom.x2)
    val py2 = max(y2, geom.y2)
    Box(px1, py1, px2, py2)
  }
}

case class Point(x: Float, y: Float) extends Geom {
  def x2: Float = x
  def y2: Float = y

  override def distanceSquared(pt: Point): Double = {
    val dx = (pt.x - x).toDouble
    val dy = (pt.y - y).toDouble
    dx * dx + dy * dy
  }
}

case class Box(x: Float, y: Float, x2: Float, y2: Float) extends Geom

object Box {

  /**
   * This is an "inside-out" box that we use as a good starting
   * value. The nice thing about this, unlike Box(0,0,0,0), is that
   * when merging with another box we don't include an artifical
   * "empty" point.
   */
  val empty: Box = {
    val s = Math.sqrt(Float.MaxValue).toFloat
    val t = s + -2.0F * s
    Box(s, s, t, t)
  }
}

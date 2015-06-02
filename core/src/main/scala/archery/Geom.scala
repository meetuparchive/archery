package archery

import scala.math.{min, max, sqrt}
import java.lang.Float.{isNaN, isInfinite}

/**
 * Geometry represents a box (or point).
 *
 * (x1, y1) is the lower left point and (x2, y2) is upper right.
 *
 * So Box(1, 2, 5, 4) is at (1, 2) and is 4 wide and 2 high.
 * Points have no width or height, so x2/y2 are equal to x/y.
 */
sealed trait Geom {
  def x: Float
  def y: Float
  def x2: Float
  def y2: Float
  def width: Float = x2 - x
  def height: Float = y2 - y

  def area: Float = width * height

  /**
   * Distance between this geometry and the given point.
   *
   * The distance is measured in terms of the closest point in the
   * geometry. For points this is obvious (there is only one point to
   * use). For boxes, this means that points contained within the box
   * (or on the perimeter) have a distance of zero.
   */
  def distance(pt: Point): Double =
    sqrt(distanceSquared(pt))

  /**
   * Distance between this geometry and the given point, squared.
   *
   * See distance() for how to interpret the distance metric. This
   * method is a bit faster than distance(), and is exposed for cases
   * where we want to compare two distances (without caring about the
   * actual distance value).
   */
  def distanceSquared(pt: Point): Double = {
    val dx = (if (pt.x < x) x - pt.x else if (pt.x < x2) 0F else pt.x - x2).toDouble
    val dy = (if (pt.y < y) y - pt.y else if (pt.y < y2) 0F else pt.y - y2).toDouble
    dx * dx + dy * dy
  }

  /**
   * Return whether all the bounds of the geometry are finite.
   *
   * "Finite" means all values except NaN and infinities.
   */
  def isFinite: Boolean =
    !(isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y) ||
      isNaN(x2) || isInfinite(x2) ||
      isNaN(y2) || isInfinite(y2))

  /**
   * Convert this Geom to a Box.
   *
   * For Boxes this is a no-op. For points, it constructs a box with
   * width and height of zero.
   */
  def toBox: Box = Box(x, y, x2, y2)

  /**
   * Get the lower-left (southwest) bound of the geometry.
   */
  def lowerLeft: Point = Point(x, y)

  /**
   * Get the upper-right (northeast) bound of the geometry.
   */
  def upperRight: Point = Point(x2, y2)

  /**
   * Returns whether this geometry contains the other.
   *
   * Containment includes the border, so points "on the edge" count as
   * contained.
   */
  def contains(geom: Geom): Boolean =
    x <= geom.x && geom.x2 <= x2 && y <= geom.y && geom.y2 <= y2

  /**
   * Returns whether this geometry intersects with the other.
   *
   * Intersection includes the border, so points "on the edge" count
   * as intersecting.
   */
  def intersects(geom: Geom): Boolean =
    x <= geom.x2 && geom.x <= x2 && y <= geom.y2 && geom.y <= y2

  /**
   * Returns whether this geometry wraps the other.
   *
   * This is the same thing as containment, but it excludes the
   * border. Points can never wrap anything, and boxes can only wrap
   * geometries with less area than they have.
   */
  def wraps(geom: Geom): Boolean =
    x < geom.x && geom.x2 < x2 && y < geom.y && geom.y2 < y2

  /**
   * Construct a Box that contains this geometry and the other.
   *
   * This will be the smallest possible box. The result of this method
   * is guaranteed to contain() both geometries.
   */
  def expand(geom: Geom): Box = {
    val px1 = min(x, geom.x)
    val py1 = min(y, geom.y)
    val px2 = max(x2, geom.x2)
    val py2 = max(y2, geom.y2)
    Box(px1, py1, px2, py2)
  }

  /**
   * Return the given geometry's area outside this geometry.
   *
   * This is equivalent to the area that would be added by expand().
   */
  def expandArea(geom: Geom): Float = {
    val px1 = min(x, geom.x)
    val py1 = min(y, geom.y)
    val px2 = max(x2, geom.x2)
    val py2 = max(y2, geom.y2)
    val a = (py2 - py1) * (px2 - px1)
    a - area
  }
}

case class Point(x: Float, y: Float) extends Geom {
  def x2: Float = x
  def y2: Float = y

  override def width: Float = 0F
  override def height: Float = 0F
  override def area: Float = 0F
  override def lowerLeft: Point = this
  override def upperRight: Point = this

  override def distanceSquared(pt: Point): Double = {
    val dx = (pt.x - x).toDouble
    val dy = (pt.y - y).toDouble
    dx * dx + dy * dy
  }

  override def isFinite: Boolean =
    !(isNaN(x) || isInfinite(x) || isNaN(y) || isInfinite(y))

  override def wraps(geom: Geom): Boolean = false
}

case class Box(x: Float, y: Float, x2: Float, y2: Float) extends Geom {
  override def toBox: Box = this
}

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

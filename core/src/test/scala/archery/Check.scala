package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{ceil, min, max}
import scala.util.Random.{nextGaussian, nextInt}
import scala.util.Try

import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object Check {

  val xmin = -14030169.0F
  val ymin =   2873645.0F
  val xmax =  -7455361.0F
  val ymax =   6226366.0F
  val dx = xmax - ymin
  val dy = ymax - ymin

  val finite: Gen[Float] =
    arbitrary[Float].suchThat(n => !n.isInfinite && !n.isNaN)

  implicit val ordering: Ordering[Entry[Int]] =
    Ordering.by(e => (e.geom.x, e.geom.y, e.geom.x2, e.geom.y2, e.value))

  implicit val arbpoint: Arbitrary[Point] =
    Arbitrary(for {
      x <- finite; y <- finite
    } yield Point(xmin + dx * (x.abs % 1.0F), ymin + dy * (y.abs % 1.0F)))

  implicit val arbbox: Arbitrary[Box] =
    Arbitrary(for {
      x1 <- finite; y1 <- finite
      x2 <- finite; y2 <- finite
    } yield Box(min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2)))

  implicit val arbgeom: Arbitrary[Geom] =
    Arbitrary(Gen.oneOf(arbitrary[Point], arbitrary[Box]))

  implicit val arbentry: Arbitrary[Entry[Int]] =
    Arbitrary(for {
      g <- arbitrary[Geom]
      n <- arbitrary[Int]
    } yield Entry(g, n))

  implicit def arbjoined[A: Arbitrary]: Arbitrary[Joined[A]] =
    Arbitrary(Gen.oneOf(
      const(Joined.empty[A]),
      arbitrary[A].map(Joined(_)),
      arbitrary[Vector[A]].map(Joined.wrap(_))))

  def build(es: Seq[Entry[Int]]): RTree[Int] =
    RTree(es: _*)

  implicit val arbrtree: Arbitrary[RTree[Int]] =
    Arbitrary(for { es <- arbitrary[List[Entry[Int]]] } yield build(es))

  def gaussianPoint(): Point =
    Point(nextGaussian.toFloat * 100 + 1000, nextGaussian.toFloat * 100 - 1000)

  def gaussianBox(): Box = {
    val xa = nextGaussian.toFloat * 100 + 1000
    val xb = nextGaussian.toFloat * 100 + 1000
    val ya = nextGaussian.toFloat * 100 - 1000
    val yb = nextGaussian.toFloat * 100 - 1000
    Box(xa min xb, ya min yb, xa max xb, ya max yb)
  }

  val mile = 1600F

  def bound(g: Geom, n: Int): Box = {
    val d = 10F * mile
    Box(g.x - d, g.y - d, g.x2 + d, g.y2 + d)
  }

  def shuffle[A](buf: ArrayBuffer[A]): Unit = {
    for (i <- 1 until buf.length) {
      val j = nextInt(i)
      val t = buf(i)
      buf(i) = buf(j)
      buf(j) = t
    }
  }
}

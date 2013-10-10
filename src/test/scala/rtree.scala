package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{ceil, min, max}
import scala.util.Random.nextFloat

import org.scalatest.matchers.ShouldMatchers
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class RTreeCheck extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  val xmin = -14030169.0F
  val ymin =   2873645.0F
  val xmax =  -7455361.0F
  val ymax =   6226366.0F
  val dx = xmax - ymin
  val dy = ymax - ymin
  
  def finite(n: Float) = !n.isInfinite && !n.isNaN

  implicit val arbpoint = Arbitrary(for {
    x <- arbitrary[Float].suchThat(finite)
    y <- arbitrary[Float].suchThat(finite)
  } yield {
    Point(xmin + dx * (x.abs % 1.0F), ymin + dy * (y.abs % 1.0F))
  })

  implicit val arbentry = Arbitrary(for {
    p <- arbitrary[Point]
    n <- arbitrary[Int]
  } yield {
    Entry(p, n)
  })

  def build(es: List[Entry[Int]]): RTree[Int] =
    es.foldLeft(RTree.empty[Int])(_ insert _)

  property("rtree.insert works") {
    forAll { (es: List[Entry[Int]]) =>
      val rt = build(es)
      rt.root.entries.toSet should be === es.toSet
    }
  }

  property("rtree.contains works") {
    forAll { (es: List[Entry[Int]], e: Entry[Int]) =>
      val rt = build(es)
      es.forall(rt.contains) should be === true

      rt.contains(e) should be === es.contains(e)
    }
  }

  property("rtree.remove works") {
    forAll { (es: List[Entry[Int]]) =>
      val rt = build(es)

      val rt2 = es.foldLeft(rt)(_ remove _)
      rt2.root.entries.isEmpty should be === true
    }
  }

  def shuffle[A](buf: ArrayBuffer[A]): Unit = {
    for (i <- 1 until buf.length) {
      val j = scala.util.Random.nextInt(i)
      val t = buf(i)
      buf(i) = buf(j)
      buf(j) = t
    }
  }

  property("rtree.remove out-of-order") {
    forAll { (es: List[Entry[Int]]) =>
      val buf = ArrayBuffer(es: _*)
      shuffle(buf)
      var rt = build(es)
      while (buf.nonEmpty) {
        buf.toSet should be === rt.entries.toSet
        val x = buf.remove(0)
        rt = rt.remove(x)
      }
      buf.toSet should be === rt.entries.toSet
    }
  }

  val mile = 1600F

  def bound(p: Point, n: Int): Box = {
    val Point(x, y) = p
    val d = 10F * mile
    Box(x - d, y - d, x + d, y + d)
  }

  property("rtree.search/count ignores nan/inf") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)
      val nil = Seq.empty[Entry[Int]]

      rt.search(Box(Float.PositiveInfinity, 3F, 9F, 9F)) should be === nil
      rt.search(Box(2F, Float.NaN, 9F, 9F)) should be === nil
      rt.search(Box(2F, 3F, Float.NegativeInfinity, 9F)) should be === nil
      rt.search(Box(2F, 3F, 9F, Float.NaN)) should be === nil

      rt.count(Box(Float.PositiveInfinity, 3F, 9F, 9F)) should be === 0
      rt.count(Box(2F, Float.NaN, 9F, 9F)) should be === 0
      rt.count(Box(2F, 3F, Float.NegativeInfinity, 9F)) should be === 0
      rt.count(Box(2F, 3F, 9F, Float.NaN)) should be === 0
    }
  }

  property("rtree.search works") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)

      val box1 = bound(p, 10)
      rt.search(box1).toSet should be === es.filter(e => box1.contains(e.pt)).toSet

      es.foreach { e =>
        val box2 = bound(e.pt, 10)
        rt.search(box2).toSet should be === es.filter(e => box2.contains(e.pt)).toSet
      }
    }
  }

  sealed trait Action {
    def test(rt: RTree[Int]): RTree[Int]
    def control(es: List[Entry[Int]]): List[Entry[Int]]
  }

  object Action {
    def run(rt: RTree[Int], es: List[Entry[Int]])(as: List[Action]): Unit =
      as match {
        case a :: as =>
          val rt2 = a.test(rt)
          val es2 = a.control(es)
          rt2.entries.toSet should be === es2.toSet
          run(rt2, es2)(as)
        case Nil =>
          ()
      }
  }

  case class Insert(e: Entry[Int]) extends Action {
    def test(rt: RTree[Int]): RTree[Int] =
      rt.insert(e)
    def control(es: List[Entry[Int]]): List[Entry[Int]] =
      e :: es
  }

  case class Remove(e: Entry[Int]) extends Action {
    def test(rt: RTree[Int]): RTree[Int] =
      if (rt.contains(e)) rt.remove(e) else rt
    def control(es: List[Entry[Int]]): List[Entry[Int]] =
      es match {
        case Nil => Nil
        case `e` :: t => t
        case h :: t => h :: control(t)
      }
  }

  implicit val arbaction = Arbitrary(for {
    e <- arbitrary[Entry[Int]]
    b <- arbitrary[Boolean]
  } yield {
    val a: Action = if (b) Insert(e) else Remove(e)
    a
  })

  property("ad-hoc rtree") {
    forAll { (es: List[Entry[Int]], as: List[Action]) =>
      Action.run(build(es), es)(as)
    }
  }
}

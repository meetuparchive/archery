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

class RTreeCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

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
      x <- finite
      y <- finite
    } yield {
      Point(xmin + dx * (x.abs % 1.0F), ymin + dy * (y.abs % 1.0F))
    })

  implicit val arbbox: Arbitrary[Box] =
    Arbitrary(for {
      x1 <- finite
      x2 <- finite
      y1 <- finite
      y2 <- finite
    } yield {
      Box(min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2))
    })

  implicit val arbentry: Arbitrary[Entry[Int]] =
    Arbitrary(for {
      either <- arbitrary[Either[Point, Box]]
      n <- arbitrary[Int]
    } yield {
      Entry(either.fold(identity, identity), n)
    })

  def build(es: Seq[Entry[Int]]): RTree[Int] =
    RTree(es: _*)

  def gaussianPoint(): Point =
    Point(nextGaussian.toFloat * 100 + 1000, nextGaussian.toFloat * 100 - 1000)

  def gaussianBox(): Box = {
    val xa = nextGaussian.toFloat * 100 + 1000
    val xb = nextGaussian.toFloat * 100 + 1000
    val ya = nextGaussian.toFloat * 100 - 1000
    val yb = nextGaussian.toFloat * 100 - 1000
    Box(xa min xb, ya min yb, xa max xb, ya max yb)
  }

  property("rtree.insert works") {
    forAll { (tpls: List[(Point, Int)]) =>
      val es = tpls.map { case (p, n) => Entry(p, n) }
      val rt1 = build(es)
      rt1.root.entries.toSet shouldBe es.toSet

      val rt2 = tpls.foldLeft(RTree.empty[Int]) { case (rt, (p, n)) =>
        rt.insert(p.x, p.y, n)
      }
      rt1 shouldBe rt2
    }
  }

  property("rtree.insertAll works") {
    forAll { (es1: List[Entry[Int]], es2: List[Entry[Int]]) =>
      val rt1 = build(es1 ++ es2)
      val rt2 = build(es1).insertAll(es2)
      rt1 shouldBe rt2
    }
  }

  property("rtree.removeAll works") {
    forAll { (es1: List[Entry[Int]], es2: List[Entry[Int]]) =>
      val rt1 = build(es1)
      val rt2 = build(es1 ++ es2).removeAll(es2)
      rt1 shouldBe rt2
    }
  }

  property("rtree.contains works") {
    forAll { (es: List[Entry[Int]], e: Entry[Int]) =>
      val rt = build(es)
      es.forall(rt.contains) shouldBe true

      rt.contains(e) shouldBe es.contains(e)
    }
  }

  property("rtree.remove works") {
    forAll { (es: List[Entry[Int]]) =>
      var rt = build(es)
      var size = rt.size
      es.foreach { e =>
        rt = rt.remove(e)
        size -= 1
        rt.size shouldBe size
      }
    }
  }

  def shuffle[A](buf: ArrayBuffer[A]): Unit = {
    for (i <- 1 until buf.length) {
      val j = nextInt(i)
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
        buf.toSet shouldBe rt.entries.toSet
        val x = buf.remove(0)
        rt = rt.remove(x)
      }
      buf.toSet shouldBe rt.entries.toSet
    }
  }

  val mile = 1600F

  def bound(g: Geom, n: Int): Box = {
    val d = 10F * mile
    Box(g.x - d, g.y - d, g.x2 + d, g.y2 + d)
  }

  property("rtree.search/count ignores nan/inf") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)
      val nil = Seq.empty[Entry[Int]]

      rt.search(Box(Float.PositiveInfinity, 3F, 9F, 9F)) shouldBe nil
      rt.search(Box(2F, Float.NaN, 9F, 9F)) shouldBe nil
      rt.search(Box(2F, 3F, Float.NegativeInfinity, 9F)) shouldBe nil
      rt.search(Box(2F, 3F, 9F, Float.NaN)) shouldBe nil

      rt.count(Box(Float.PositiveInfinity, 3F, 9F, 9F)) shouldBe 0
      rt.count(Box(2F, Float.NaN, 9F, 9F)) shouldBe 0
      rt.count(Box(2F, 3F, Float.NegativeInfinity, 9F)) shouldBe 0
      rt.count(Box(2F, 3F, 9F, Float.NaN)) shouldBe 0
    }
  }

  property("rtree.search works") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)

      val box1 = bound(p, 10)
      rt.search(box1).toSet shouldBe es.filter(e => box1.contains(e.geom)).toSet

      es.foreach { e =>
        val box2 = bound(e.geom, 10)
        rt.search(box2).toSet shouldBe es.filter(e => box2.contains(e.geom)).toSet
      }
    }
  }

  property("rtree.searchIntersection works") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)

      val box1 = bound(p, 10)
      rt.searchIntersection(box1).toSet shouldBe es.filter(e => box1.intersects(e.geom)).toSet

      es.foreach { e =>
        val box2 = bound(e.geom, 10)
        rt.searchIntersection(box2).toSet shouldBe es.filter(e => box2.intersects(e.geom)).toSet
      }
    }
  }

  property("rtree.nearest works") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)
      if (es.isEmpty) {
        rt.nearest(p) shouldBe None
      } else {
        val e = es.min(Ordering.by((e: Entry[Int]) => e.geom.distance(p)))
        val d = e.geom.distance(p)
        // it's possible that several points are tied for closest
        // in these cases, the distances still must be equal.
        rt.nearest(p).map(_.geom.distance(p)) shouldBe Some(d)
      }
    }
  }

  property("rtree.nearestK works") {
    forAll { (es: List[Entry[Int]], p: Point, k0: Int) =>
      val k = (k0 % 1000).abs
      val rt = build(es)

      val as = es.map(_.geom.distance(p)).sorted.take(k).toVector
      val bs = rt.nearestK(p, k).map(_.geom.distance(p))
      as shouldBe bs
    }
  }

  property("rtree.count works") {
    val es = (1 to 10000).map(n => Entry(gaussianPoint, n))
    val rt = build(es)
    val box = gaussianBox
    rt.count(box) shouldBe es.filter(e => box.contains(e.geom)).size
    val box2 = Box(1e10F, 1e10F, 1e11F, 1e11F)
    rt.count(box2) shouldBe es.filter(e => box2.contains(e.geom)).size
  }

  property("rtree.map works") {
    forAll { (es: List[Entry[Int]]) =>
      val f = (x: Int) => x + 1
      val rt = build(es)
      val es2 = es.map(e => Entry(e.geom, f(e.value)))
      rt.map(f) shouldBe build(es2)
    }
  }

  property("rtree equals/hashCode work") {
    forAll { (es1: List[Entry[Int]], e: Entry[Int]) =>
      val es2 = ArrayBuffer(es1: _*)
      shuffle(es2)
      val (rt1, rt2) = (build(es1), build(es2))
      rt1 shouldBe rt2
      rt1.hashCode shouldBe rt2.hashCode
      rt1 should not be (999)

      val rt3 = rt1.insert(e)
      rt3 should not be (rt1)
      // this should only have a very small chance of failing,
      // assuming RTree#hashCode is a good hashing function.
      rt3.hashCode should not be (rt1.hashCode)
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
          rt2.entries.toSet shouldBe es2.toSet
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

  implicit val arbaction: Arbitrary[Action] =
    Arbitrary(for {
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

  property("dense rtree") {
    val es = (1 to 100000).map(n => Entry(gaussianPoint, n))
    val rt = build(es)
    rt.size shouldBe es.size
    es.forall(rt.contains) shouldBe true

    var rt0 = rt
    var size = rt.size
    es.foreach { e =>
      rt0 = rt0.remove(e)
      size -= 1
      rt0.size shouldBe size
    }
  }

  property("pretty-printing") {
    forAll { (es: List[Entry[Int]]) =>
      val rt = build(es)
      Try(rt.pretty).isSuccess shouldBe true
    }
  }
}

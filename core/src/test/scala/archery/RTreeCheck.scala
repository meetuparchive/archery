package archery

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Arbitrary.arbitrary

import Check._

class RTreeCheck extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  property("rtree.insert works") {
    forAll { (tpls: List[(Point, Int)]) =>
      val es = tpls.map { case (p, n) => Entry(p, n) }
      val rt1 = build(es)
      rt1.entries.toSet shouldBe es.toSet
      rt1.values.toSet shouldBe tpls.map(_._2).toSet

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
      es.forall {
        case Entry(Point(x, y), v) => rt.contains(x, y, v)
        case _ => true
      } shouldBe true
      rt.contains(e) shouldBe es.contains(e)
    }
  }

  property("rtree.remove works") {
    forAll { (es: List[Entry[Int]]) =>
      var rt = build(es)
      var size = rt.size

      // very small chance of failing
      rt shouldBe rt.remove(Entry(Point(1234F, 5678F), 0xbad))

      es.foreach { e =>
        rt = rt.remove(e)
        size -= 1
        rt.size shouldBe size
      }
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

  property("rtree.search/count ignores nan/inf") {
    forAll { (rt: RTree[Int]) =>
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
      val results1 = rt.search(box1).toSet
      results1 shouldBe es.filter(e => box1.contains(e.geom)).toSet

      val f = (e: Entry[Int]) => e.value % 2 == 0
      val results1f = rt.search(box1, f).toSet
      results1f shouldBe es.filter(e => box1.contains(e.geom) && f(e)).toSet
      results1f shouldBe results1.filter(f)

      val g = (n: Long, e: Entry[Int]) => n + e.value

      es.foreach { e =>
        val box2 = bound(e.geom, 10)
        val results = rt.search(box2)
        results.toSet shouldBe es.filter(e => box2.contains(e.geom)).toSet

        val x = results.foldLeft(0L)(g)
        val y = rt.foldSearch(box2, 0L)(g)
        if (x != y) {
          println(box2)
          println(rt.pretty)
          println(rt.root.searchIterator(box2, _ => true).toList)
        }
        x shouldBe y
      }
    }
  }

  property("rtree.searchIntersection works(Box)") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)

      val box1 = bound(p, 10)
      rt.searchIntersection(box1).toSet shouldBe es.filter(e => box1.intersects(e.geom)).toSet

      val f = (e: Entry[Int]) => e.value % 3 != 1
      rt.searchIntersection(box1, f).toSet shouldBe es.filter(e => box1.intersects(e.geom) && f(e)).toSet

      es.foreach { e =>
        val box2 = bound(e.geom, 10)
        rt.searchIntersection(box2).toSet shouldBe es.filter(e => box2.intersects(e.geom)).toSet
      }
    }
  }

  property("rtree.searchIntersection works(Point)") {
    forAll { (es: List[Entry[Int]], p: Point) =>
      val rt = build(es)

      rt.searchIntersection(p).toSet shouldBe es.filter(e => p.intersects(e.geom)).toSet

      val f = (e: Entry[Int]) => e.value % 3 != 1
      rt.searchIntersection(p, f).toSet shouldBe es.filter(e => p.intersects(e.geom) && f(e)).toSet

      es.foreach { e =>
        rt.searchIntersection(p).toSet shouldBe es.filter(e => p.intersects(e.geom)).toSet
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

  property("sane toString/pretty-printing") {
    forAll { (rt: RTree[Int]) =>
      rt.toString.length should be < 20
      Try(rt.pretty).isSuccess shouldBe true
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

  property("prove coverage of inlined constant") {
    Constants.MaxEntries shouldBe 50
  }
}

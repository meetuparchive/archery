package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}
import scala.util.Random.{nextFloat, nextInt}

object Main {
  def nextPoint: Point =
    Point(100F * nextFloat, 100F * nextFloat)

  def nextEntries(n: Int): Array[Entry[Int]] =
    (0 until n).map(i => Entry(nextPoint, i)).toArray

  def nextTree(n: Int): RTree[Int] =
    nextEntries(n).foldLeft(RTree.empty[Int])((rt, e) => rt.insert(e))

  def timer[A](f: => A): (A, Long) = {
    val t0 = System.nanoTime
    val a = f
    val t = (System.nanoTime - t0) / 1000L
    (a, t)
  }

  def timeNew(es: Array[Entry[Int]]): (RTree[Int], Long) =
    timer {
      var rt = RTree.empty[Int]
      var i = 0
      val t0 = System.nanoTime
      while (i < es.length) {
        rt = rt.insert(es(i))
        i += 1
      }
      rt
    }

  def benchMercator() {
    val xmin = -14030169.0F
    val ymin =   2873645.0F
    val xmax =  -7455361.0F
    val ymax =   6226366.0F
    val dx = xmax - ymin
    val dy = ymax - ymin

    def nextPoint = Point(nextFloat * dx + xmin, nextFloat * dy + ymin)
    def nextEntry(n: Int) = Entry(nextPoint, n)

    val mile = 1600F

    def nextBox(n: Int) = {
      val Point(x, y) = nextPoint
      val d = n * mile
      Box(x - d, y - d, x + d, y + d)
    }

    val sz = 1000 * 1000
    val (rt, t1) = timer {
      (0 until sz).foldLeft(RTree.empty[Int]) { (rt, i) =>
        rt.insert(nextEntry(i))
      }
    }
    println(s"built r-tree ($sz points) in $t1 us")

    val num = 1000
    val boxes = (0 until num).map(_ => nextBox(10))
    val (n, t2) = timer {
      var n = 0
      boxes.foreach { b =>
        n += rt.search(b).length
      }
      n
    }
    val avg = "%.1f" format (t2.toDouble / num)
    println(s"$num searches found $n events in $t2 us (avg: $avg us)")
  }

  def benchLoad() {
    val sz = 1000000
    val es = nextEntries(sz)
    (0 until 2).foreach(_ => timeNew(es))
    val ts = (0 until 3).map(_ => timeNew(es)._2)
    val avg = ts.sum / 3
    println(s"$avg, $ts")
  }

  def main(args: Array[String]) {
    //benchLoad()
    benchMercator()
  }
}

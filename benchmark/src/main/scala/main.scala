package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}
import scala.util.Random.{nextFloat, nextInt, nextGaussian}

import ichi.bench.Thyme

object Main {
  val xmin, ymin = -5000F
  val xmax, ymax = 5000F
  val dx, dy = 10000F
  val size = 1000000
  val num = 1000
  val radius = 10

  val entries = (0 until size).map(n => Entry(nextPoint, n)).toArray
  val extra = (0 until num).map(n => Entry(nextPoint, n + size)).toArray
  val boxes = (0 until num).map(_ => nextBox(radius))

  // generate values in [-5F, 5F], mean 0F with stddev 1F
  def nextF: Float = {
    val n = nextGaussian.toFloat
    if (n < -5F) -5F else if (n > 5F) 5F else n
  }

  // cluster points around (0, 0)
  def nextPoint: Point =
    Point(1000F * nextF, 1000F * nextF)

  // generate box with radius r
  def nextBox(r: Int): Box = {
    val Point(x, y) = nextPoint
    Box(x - r, y - r, x + r, y + r)
  }

  def main(args: Array[String]) {
    val th = Thyme.warmedBench(verbose = print)
    println(s"\nbuilding rtree from $size entries")
    val rt = th.pbench {
      RTree(entries: _*)
      //entries.foldLeft(RTree.empty[Int])(_ insert _)
    }

    println(s"\ndoing $num random searches (radius: $radius)")
    val n1 = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.search(b).length)
    }
    println(s"found $n1 results")

    println(s"\ndoing $num random searches with filter (radius: $radius)")
    val nx = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.search(b, _ => true).length)
    }
    println(s"found $nx results")

    println(s"\ndoing $num counts")
    val n2 = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.count(b))
    }
    println(s"found $n2 results")

    println(s"\nremoving $num entries")
    th.pbench {
      entries.take(num).foldLeft(rt)(_ remove _)
    }

    println(s"\ninserting $num entries")
    th.pbench {
      extra.foldLeft(rt)(_ insert _)
    }
  }
}

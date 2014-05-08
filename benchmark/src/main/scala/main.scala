package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}
import scala.util.Random.{nextFloat, nextInt, nextGaussian}

import ichi.bench.Thyme

import geotrellis.Extent
import geotrellis.feature._

case class HybridTree[A](base: SpatialIndex[Entry[A]], deleted: Set[Entry[A]], added: RTree[A]) {
  def insert(e: Entry[A]): HybridTree[A] =
    HybridTree(base, deleted, added insert e)

  def remove(e: Entry[A]): HybridTree[A] =
    HybridTree(base, deleted + e, added remove e)

  def search(b: Box): Seq[Entry[A]] = {
    val xs = base.pointsInExtent(Extent(b.x, b.y, b.x2, b.y2)).filterNot(deleted)
    val ys = added.search(b)
    xs ++ ys
  }
}

object HybridTree {
  def apply[A](entries: Entry[A]*): HybridTree[A] = {
    val base = SpatialIndex(entries)(p => (p.geom.x, p.geom.y))
    HybridTree(base, Set.empty[Entry[A]], RTree.empty[A])
  }
}

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
    println(s"\narchery: building tree from $size entries")
    val rt = th.pbench {
      RTree(entries: _*)
    }

    println(s"\ngeotrellis: building tree from $size entries")
    val rt_geotrellis = th.pbench {
      val index = SpatialIndex(entries)(p => (p.geom.x, p.geom.y))
      index
    }

    println(s"\nhybrid: building tree from $size entries")
    val rt_hybrid = th.pbench {
      HybridTree(entries: _*)
    }

    println(s"\narchery: doing $num random searches (radius: $radius)")
    val n1 = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.search(b).length)
    }
    println(s"  found $n1 results")

    println(s"\ngeotrellis: doing $num random searches (radius: $radius)")
    val n1_gt = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt_geotrellis.pointsInExtent(geotrellis.Extent(b.x, b.y, b.x2, b.y2)).length)       
    }
    println(s"  found $n1_gt results")

    println(s"\ndoing $num random searches with filter (radius: $radius)")
    val nx = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.search(b, _ => true).length)
    }
    println(s"found $nx results")

    println(s"\nhybrid: doing $num random searches (radius: $radius)")
    val n1_h = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt_hybrid.search(b).length)
    }
    println(s"  found $n1_h results")

    println(s"\narchery: doing $num counts")
    val n2 = th.pbench {
      boxes.foldLeft(0)((n, b) => n + rt.count(b))
    }
    println(s"found $n2 results")

    println(s"\narchery: removing $num entries")
    th.pbench {
      entries.take(num).foldLeft(rt)(_ remove _)
    }

    // println(s"\nhybrid: removing $num entries")
    // th.pbench {
    //   entries.take(num).foldLeft(rt_hybrid)(_ remove _)
    // }

    println(s"\narchery: inserting $num entries")
    th.pbench {
      extra.foldLeft(rt)(_ insert _)
    }

    println(s"\nhybrid: inserting $num entries")
    th.pbench {
      extra.foldLeft(rt_hybrid)(_ insert _)
    }
  }
}

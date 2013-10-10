package archery

import scala.collection.mutable.ArrayBuffer
import scala.math.{min, max}

/**
 * Some useful constants that we don't want to hardcode.
 */
object Constants {
  @inline final val MaxEntries = 50
}

sealed trait Joined[A] extends Iterable[A] {
  override def isEmpty: Boolean = false
  def iterator: Iterator[A]
  def ++(that: Joined[A]): Joined[A] =
    if (that.isEmpty) this else Joined.Concat(this, that)
}

object Joined {
  def empty[A]: Joined[A] = Wrapped(Vector.empty)
  def apply[A](a: A): Joined[A] = Single(a)
  def wrap[A](as: Vector[A]): Joined[A] = Wrapped(as)

  case class Single[A](a: A) extends Joined[A] {
    def iterator: Iterator[A] = Iterator(a)
  }
  case class Wrapped[A](as: Vector[A]) extends Joined[A] {
    override def isEmpty: Boolean = as.isEmpty
    def iterator: Iterator[A] = as.iterator
    override def ++(that: Joined[A]): Joined[A] =
      if (this.isEmpty) that else if (that.isEmpty) this else Joined.Concat(this, that)
  }
  case class Concat[A](x: Joined[A], y: Joined[A]) extends Joined[A] {
    def iterator: Iterator[A] = x.iterator ++ y.iterator
  }
}

import Constants._

/**
 * Geometry represents a box (or point).
 * 
 * The box/point distinction is really only relevant because
 * point-in-box searches are a lot faster. Otherwise they have
 * basically the same API.
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

  def isFinite: Boolean = !(isNaN(x) || isInfinite(x) ||
    isNaN(y) || isInfinite(y) ||
    isNaN(x2) || isInfinite(x2) ||
    isNaN(y2) || isInfinite(y2))

  def toBox: Box = Box(x, y, x2, y2)

  def lowerLeft: Point = Point(x, y)

  def upperRight: Point = Point(x2, y2)

  def contains(pt: Point): Boolean =
    x <= pt.x && pt.x <= x2 && y <= pt.y && pt.y <= y2

  def intersects(box: Box): Boolean =
    x <= box.x2 && box.x <= x2 && y <= box.y2 && box.y <= y2

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

/**
 * Abstract data type that has a geom member.
 * 
 * This generalizes Node[A] (the nodes of the tree) and Entry[A] (the
 * values being put in the tree).
 */
sealed trait Member {
  def geom: Geom
}

/**
 * Abstract data type for nodes of the tree.
 * 
 * There are two types of Node: Branch and Leaf. Confusingly, leaves
 * don't actaully hold values, but rather a leaf contains a sequence
 * of entries. This design is commmon to RTree implementations and it
 * seemed like a good idea to keep the nomenclature the same.
 */
sealed trait Node[A] extends Member {
  def box: Box
  def geom: Geom = box

  /**
   * Put all the entries this node contains (directly or indirectly)
   * into a vector. Obviously this could be quite large in the case of
   * a root node, so it should not be used for traversals.
   */
  def entries: Vector[Entry[A]] = {
    val buf = ArrayBuffer.empty[Entry[A]]
    def recur(node: Node[A]): Unit = node match {
      case Leaf(children, _) =>
        buf ++= children
      case Branch(children, _) =>
        children.foreach(recur)
    }
    recur(this)
    buf.toVector
  }

  /**
   * Returns an iterator over all the entires this node contains
   * (directly or indirectly). Since nodes are immutable there is no
   * concern over concurrent updates while using the iterator.
   */
  def iterator: Iterator[Entry[A]] = this match {
    case Leaf(children, _) =>
      children.iterator
    case Branch(children, _) =>
      children.iterator.flatMap(_.iterator)
  }

  /**
   * Method to pretty-print an r-tree.
   *
   * This method should only be called on small-ish trees! It will
   * print one line for every branch, leaf, and entry, so for a tree
   * with thousands of entries this will result in a very large
   * string!
   */
  def pretty: String = {
    def prettyRecur(node: Node[A], i: Int, sb: StringBuilder): Unit = {
      val pad = " " * i
      val a = node.box.area
      node match {
        case lf @ Leaf(children, box) =>
          val pad2 = " " * (i + 1)
          sb.append(s"$pad leaf $a $box:\n")
          children.foreach { case Entry(pt, value) =>
            sb.append(s"$pad2 entry $pt: $value\n")
          }
        case Branch(children, box) =>
          sb.append(s"$pad branch $a $box:\n")
          children.foreach(c => prettyRecur(c, i + 1, sb))
      }
    }
    val sb = new StringBuilder
    prettyRecur(this, 0, sb)
    sb.toString
  }

  /**
   * Insert a new Entry into the tree.
   * 
   * Since this node is immutable, the method will return a
   * replacement. There are two possible situations:
   *
   * 1. We can replace this node with a new node. This is the common
   *    case.
   * 
   * 2. This node was already "full", so we can't just replace it with
   *    a single node. Instead, we will split this node into
   *    (presumably) two new nodes, and return a vector of them.
   * 
   * The reason we are using vector here is that it simplifies the
   * implementation, and also because eventually we may support bulk
   * insertion, where more than two nodes might be returned.
   */
  def insert(entry: Entry[A]): Either[Vector[Node[A]], Node[A]] = {
    this match {
      case Leaf(children, box) =>
        val cs = children :+ entry
        if (cs.length <= MaxEntries) {
          Right(Leaf(cs, box.expand(entry.pt)))
        } else {
          Left(Node.splitLeaf(cs))
        }

      case Branch(children, box) =>
        assert(children.length > 0)

        // here we need to find the "best" child to put the entry
        // into. we define that as the child that needs to add the
        // least amount of area to its own bounding box to accomodate
        // the new entry.
        //
        // the results are "node", the node to add to, and "n", the
        // position of that node in our vector.
        val pt = entry.pt
        var node = children(0)
        var n = 0
        var area = node.box.expandArea(pt)
        var i = 1
        while (i < children.length) {
          val curr = children(i)
          val a = curr.box.expandArea(pt)
          if (a < area) {
            area = a
            n = i
            node = curr
          }
          i += 1
        }

        // now we perform the actual insertion into the node. as
        // stated above, that node will either return a single new
        // node (Right) or a vector of new nodes (Left).
        node.insert(entry) match {
          case Left(rs) =>
            val cs = children.take(n) ++ children.drop(n + 1) ++ rs
            if (cs.length <= MaxEntries) {
              val b = rs.foldLeft(box)(_ expand _.box)
              Right(Branch(cs, b))
            } else {
              Left(Node.splitBranch(cs))
            }
          case Right(r) =>
            val cs = children.updated(n, r)
            if (cs.length <= MaxEntries) {
              Right(Branch(children.updated(n, r), box.expand(r.box)))
            } else {
              Left(Node.splitBranch(cs))
            }
        }
    }
  }

  /**
   * Determine if we need to try contracting our bounding box based on
   * the loss of 'geom'. If so, use the by-name parameter 'regen' to
   * recalculate. Since regen is by-name, it won't be evaluated unless
   * we need it.
   */
  def contract(gone: Geom, regen: => Box): Box =
    if (box.wraps(gone)) box else regen

  /**
   * Remove this entry from the tree.
   * 
   * The implementations for Leaf and Branch are somewhat involved, so
   * they are defined in each subclass.
   * 
   * The return value can be understood as follows:
   * 
   * 1. None: the entry was not found in this node. This is the most
   *    common case.
   * 
   * 2. Some((es, None)): the entry was found, and this node was
   *    removed (meaning after removal it had too few other
   *    children). The 'es' vector are entries that need to be readded
   *    to the RTree.
   * 
   * 3. Some((es, Some(node))): the entry was found, and this node
   *    should be replaced by 'node'. Like above, the 'es' vector
   *    contains entries that should be readded.
   * 
   * Because adding entries may require rebalancing the tree, we defer
   * the insertions until after the removal is complete and then readd
   * them in RTree. While 'es' will usually be quite small, it's
   * possible that in some cases it may be very large.
   * 
   * TODO: To avoid allocating large vectors, we could create a custom
   * collection which could be pieced together out of vectors and
   * singletons.
   */
  def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])]

  /**
   * Performs a search for all entries in the search space.
   * 
   * Points on the boundary of the search space will be included.
   */
  def search(space: Box): Seq[Entry[A]] = {
    if (!space.isFinite) return Seq.empty

    val buf = ArrayBuffer.empty[Entry[A]]
    def recur(node: Node[A]): Unit = node match {
      case Leaf(children, box) =>
        children.foreach { c =>
          if (space.contains(c.pt)) buf.append(c)
        }
      case Branch(children, box) =>
        children.foreach { c =>
          if (space.intersects(box)) recur(c)
        }
    }
    if (space.intersects(box)) recur(this)
    buf
  }

  def count(space: Box): Int = {
    if (!space.isFinite) return 0

    def recur(node: Node[A]): Int = node match {
      case Leaf(children, box) =>
        var n = 0
        var i = 0
        while (i < children.length) {
          if (space.contains(children(i).pt)) n += 1
          i += 1
        }
        n
      case Branch(children, box) =>
        var n = 0
        var i = 0
        while (i < children.length) {
          val c = children(i)
          if (space.intersects(c.box)) n += recur(c)
          i += 1
        }
        n
    }
    if (space.intersects(box)) recur(this) else 0
  }

  /**
   * Determine if entry is contained in the tree.
   * 
   * This method depends upon reasonable equality for A. It can only
   * match an Entry(pt, x) if entry.value == x.value.
   */
  def contains(entry: Entry[A]): Boolean =
    search(entry.pt.toBox).contains(entry)
}

case class Branch[A](children: Vector[Node[A]], box: Box) extends Node[A] {

  def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])] = {
    def loop(i: Int): Option[(Joined[Entry[A]], Option[Node[A]])] =
      if (i < children.length) {
        val child = children(i)
        child.remove(entry) match {
          case None =>
            loop(i + 1)

          case Some((es, None)) =>
            if (children.length == 1) {
              Some((es, None))
            } else if (children.length == 2) {
              Some((Joined.wrap(children(1 - i).entries) ++ es, None))
            } else {
              val cs = children.take(i) ++ children.drop(i + 1)
              val b = contract(child.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
              Some((es, Some(Branch(cs, b))))
            }
            
          case Some((es, Some(node))) =>
            val cs = children.updated(i, node)
            val b = contract(child.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
            Some((es, Some(Branch(cs, b))))
        }
      } else {
        None
      }

    if (!box.contains(entry.pt)) None else loop(0)
  }
}


case class Leaf[A](children: Vector[Entry[A]], box: Box) extends Node[A] {

  def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])] = {
    if (!box.contains(entry.pt)) return None
    val i = children.indexOf(entry)
    if (i < 0) {
      None
    } else if (children.length == 1) {
      Some((Joined.empty[Entry[A]], None))
    } else if (children.length == 2) {
      Some((Joined(children(1 - i)), None))
    } else {
      val cs = children.take(i) ++ children.drop(i + 1)
      val b = contract(entry.pt, cs.foldLeft(Box.empty)(_ expand _.geom))
      Some((Joined.empty[Entry[A]], Some(Leaf(cs, b))))
    }
  }
}

/**
 * Represents a point with a value.
 * 
 * We frequently use value.== so it's important that A have a
 * reasonable equality definition. Otherwise things like remove and
 * contains may not work very well.
 */
case class Entry[A](pt: Point, value: A) extends Member {
  def geom: Geom = pt
}

object Node {

  def empty[A]: Node[A] = Leaf(Vector.empty, Box.empty)

  /**
   * Splits the children of a leaf node.
   * 
   * See splitter for more information.
   */
  def splitLeaf[A](children: Vector[Entry[A]]): Vector[Leaf[A]] = {
    val ((es1, box1), (es2, box2)) = splitter(children)
    Vector(Leaf(es1, box1), Leaf(es2, box2))
  }

  /**
   * Splits the children of a branch node.
   * 
   * See splitter for more information.
   */
  def splitBranch[A](children: Vector[Node[A]]): Vector[Branch[A]] = {
    val ((ns1, box1), (ns2, box2)) = splitter(children)
    Vector(Branch(ns1, box1), Branch(ns2, box2))
  }

  /**
   * Splits a collection of members into two new collections, grouped
   * according to the rtree algorithm.
   * 
   * The results (a vector and a bounding box) will be used to create
   * new nodes.
   * 
   * The goal is to minimize the area and overlap of the pairs'
   * bounding boxes. We are using a linear seeding strategy since it
   * is simple and has worked well for us in the past.
   */
  def splitter[M <: Member](children: Vector[M]): ((Vector[M], Box), (Vector[M], Box)) = {
    val buf = ArrayBuffer(children: _*)
    val (seed1, seed2) = pickSeeds(buf)

    var box1: Box = seed1.geom.toBox
    var box2: Box = seed2.geom.toBox
    val nodes1 = ArrayBuffer(seed1)
    val nodes2 = ArrayBuffer(seed2)

    def add1(node: M) { nodes1 += node; box1 = box1.expand(node.geom) }
    def add2(node: M) { nodes2 += node; box2 = box2.expand(node.geom) }

    while (buf.nonEmpty) {

      if (nodes1.length >= 2 && nodes2.length + buf.length <= 2) {
        // We should put the remaining buffer all in one bucket.
        nodes2 ++= buf
        box2 = buf.foldLeft(box2)(_ expand _.geom)
        buf.clear()

      } else if (nodes2.length >= 2 && nodes1.length + buf.length <= 2) {
        // We should put the remaining buffer all in the other bucket.
        nodes1 ++= buf
        box1 = buf.foldLeft(box1)(_ expand _.geom)
        buf.clear()

      } else {
        // We want to find the bucket whose bounding box requires the
        // smallest increase to contain this member. If both are the
        // same, we look for the bucket with the smallest area. If
        // those are the same, we flip a coin.
        val node = buf.remove(buf.length - 1)
        val e1 = box1.expandArea(node.geom)
        val e2 = box2.expandArea(node.geom)
        if (e1 < e2) {
          add1(node)
        } else if (e2 < e1) {
          add2(node)
        } else {
          val b1 = box1.expand(node.geom)
          val b2 = box2.expand(node.geom)
          val a1 = b1.area
          val a2 = b2.area
          if (a1 < a2) {
            add1(node)
          } else if (a2 < a1) {
            add2(node)
          } else if (Math.random() > 0.5) {
            add1(node)
          } else {
            add2(node)
          }
        }
      }
    }
    ((nodes1.toVector, box1), (nodes2.toVector, box2))
  }

  /**
   * Given a collection of members, we want to find the two that have
   * the greatest distance from each other in some dimension. This is
   * the "linear" strategy.
   * 
   * Other strategies (like finding the greatest distance in both
   * dimensions) might give better seeds but would be slower. This
   * seems to work OK for now.
   */
  def pickSeeds[M <: Member](nodes: ArrayBuffer[M]): (M, M) = {

    // find the two geometries that have the most space between them
    // in this particular dimension. the sequence is (lower, upper) points
    def handleDimension(pairs: IndexedSeq[(Float, Float)]): (Float, Int, Int) = {
      var z1, z3 = pairs(0)._1
      var z2, z4 = pairs(0)._2
      var left = 0
      var right = 0
      var i = 1
      while (i < pairs.length) {
        val (x1, x2) = pairs(i)
        if (x1 < z1) z1 = x1
        if (x2 > z4) z4 = x2
        if (x1 > z3) { z3 = x1; right = i }
        if (x2 < z2) { z2 = x2; left = i }
        i += 1
      }
      if (z4 != z1) ((z3 - z2) / (z4 - z1), left, right) else (0.0F, 0, 1)
    }

    // get back the maximum distance in each dimension, and the coords
    val (w1, i1, j1) = handleDimension(nodes.map(n => (n.geom.x, n.geom.x2)))
    val (w2, i2, j2) = handleDimension(nodes.map(n => (n.geom.y, n.geom.y2)))

    // figure out which dimension "won"
    val (i, j) = if (w1 > w2) (i1, j1) else (i2, j2)

    // remove these nodes and return them
    // make sure to remove the larger index first.
    val (a, b) = if (i > j) (i, j) else (j, i)
    val node1 = nodes.remove(a)
    val node2 = nodes.remove(b)
    (node1, node2)
  }
}

/**
 * This is the magnificent RTree, which makes searching ad-hoc
 * geographic data fast and fun.
 * 
 * The RTree wraps a node called 'root' that is the actual root of the
 * tree structure. RTree also keeps track of the total size of the
 * tree (something that individual nodes don't do).
 */
case class RTree[A](root: Node[A], size: Int) {

  /**
   * Insert a value into the tree at (x, y), returning a new tree.
   */
  def insert(x: Float, y: Float, value: A): RTree[A] =
    insert(Entry(Point(x, y), value))

  /**
   * Insert an entry into the tree, returning a new tree.
   */
  def insert(entry: Entry[A]): RTree[A] = {
    val r = root.insert(entry) match {
      case Left(rs) => Branch(rs, rs.foldLeft(Box.empty)(_ expand _.box))
      case Right(r) => r
    }
    RTree(r, size + 1)
  }

  /**
   * Remove a value found at (x, y) from the tree.
   */
  def remove(x: Float, y: Float, value: A): RTree[A] =
    remove(Entry(Point(x, y), value))

  /**
   * Remove an entry from the tree, returning a new tree.
   * 
   * If the entry was not present, this method will throw an error.
   */
  def remove(entry: Entry[A]): RTree[A] =
    root.remove(entry) match {
      case None =>
        sys.error("wat")
      case Some((es, None)) =>
        es.foldLeft(RTree.empty[A])(_ insert _)
      case Some((es, Some(node))) =>
        es.foldLeft(RTree(node, size - 1))(_ insert _)
    }

  /**
   * Performs a search for all entries in the search space.
   * 
   * Points on the boundary of the search space will be included.
   */
  def search(lat1: Float, lon1: Float, lat2: Float, lon2: Float): Seq[A] =
    search(Box(lat1, lon1, lat2, lon2)).map(_.value)

  /**
   * Return a sequence of all entries found in the given search space.
   */
  def search(space: Box): Seq[Entry[A]] =
    root.search(space)

  /**
   * Return a count of all entries found in the given search space.
   */
  def count(space: Box): Int =
    root.count(space)

  /**
   * Return whether or not the value exists in the tree at (x, y).
   */
  def contains(x: Float, y: Float, value: A): Boolean =
    root.contains(Entry(Point(x, y), value))

  /**
   * Return whether or not the given entry exists in the tree.
   */
  def contains(entry: Entry[A]): Boolean =
    root.contains(entry)

  /**
   * Return an iterator over all entries in the tree.
   */
  def entries: Iterator[Entry[A]] =
    root.iterator

  /**
   * Return an iterator over all values in the tree.
   */
  def values: Iterator[A] =
    entries.map(_.value)

  /**
   * Return a nice depiction of the tree.
   * 
   * This method should only be called on small-ish trees! It will
   * print one line for every branch, leaf, and entry, so for a tree
   * with thousands of entries this will result in a very large
   * string!
   */
  def pretty: String = root.pretty
}

object RTree {

  /**
   * Construct an empty RTree.
   */
  def empty[A]: RTree[A] = new RTree(Node.empty[A], 0)
}

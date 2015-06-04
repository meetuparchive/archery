package archery

import scala.collection.mutable.{ArrayBuffer, PriorityQueue}
import scala.math.{min, max}

/**
 * Some useful constants that we don't want to hardcode.
 */
object Constants {
  // $COVERAGE-OFF$
  @inline final val MaxEntries = 50
  // $COVERAGE-ON$
}

import Constants._

/**
 * Abstract data type that has a geom element.
 *
 * This generalizes Node[A] (the nodes of the tree) and Entry[A] (the
 * values being put in the tree). It functions like a structural type
 * (but isn't one, because structural types are slow).
 */
sealed abstract class HasGeom {
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
sealed abstract class Node[A] extends HasGeom { self =>
  def box: Box
  def geom: Geom = box

  def children: Vector[HasGeom]

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
          Right(Leaf(cs, box.expand(entry.geom)))
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
        val pt = entry.geom
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
   */
  def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])]

  /**
   * Search for all entries contained in the search space.
   *
   * Points on the boundary of the search space will be included.
   */
  def search(space: Box, f: Entry[A] => Boolean): Seq[Entry[A]] =
    genericSearch(space, space.contains, f)

  /**
   * Search for all entries intersecting the search space.
   *
   * Points on the boundary of the search space will be included.
   */
  def searchIntersection(space: Box, f: Entry[A] => Boolean): Seq[Entry[A]] =
    genericSearch(space, space.intersects, f)

  /**
   * Search for all entries given a search space, spatial checking
   * function, and criteria function.
   *
   * This method abstracts search and searchIntersection, where the
   * `check` function is either space.contains or space.intersects,
   * respectively.
   */
  def genericSearch(space: Box, check: Geom => Boolean, f: Entry[A] => Boolean): Seq[Entry[A]] =
    if (!space.isFinite) Nil else {
      val buf = ArrayBuffer.empty[Entry[A]]
      def recur(node: Node[A]): Unit = node match {
        case Leaf(children, box) =>
          children.foreach { c =>
            if (check(c.geom) && f(c)) buf.append(c)
          }
        case Branch(children, box) =>
          children.foreach { c =>
            if (space.intersects(box)) recur(c)
          }
      }
      if (space.intersects(box)) recur(this)
      buf
    }

  /**
   * Combine the results of a search(space) into a single result.
   */
  def foldSearch[B](space: Box, init: B)(f: (B, Entry[A]) => B): B =
    searchIterator(space, _ => true).foldLeft(init)(f)

  /**
   * Return an iterator over the results of a search.
   *
   * This produces the same elements as search(space, f).iterator(),
   * without having to build an entire vector at once.
   */
  def searchIterator(space: Box, f: Entry[A] => Boolean): Iterator[Entry[A]] =
    if (children.isEmpty || !box.intersects(space)) {
      Iterator.empty
    } else {
      this match {
        case Leaf(cs, _) =>
          cs.iterator.filter(c => space.contains(c.geom) && f(c))
        case Branch(cs, _) =>
          cs.iterator.flatMap(c => c.searchIterator(space, f))
      }
    }

  /**
   * Find the closest entry to `pt` that is within `d0`.
   *
   * This method will either return Some((distance, entry)) or None.
   */
  def nearest(pt: Point, d0: Double): Option[(Double, Entry[A])] = {
    var dist: Double = d0
    var result: Option[(Double, Entry[A])] = None
    this match {
      case Leaf(children, box) =>
        children.foreach { entry =>
          val d = entry.geom.distance(pt)
          if (d < dist) {
            dist = d
            result = Some((d, entry))
          }
        }
      case Branch(children, box) =>
        val cs = children.map(node => (node.box.distance(pt), node)).sortBy(_._1)
        cs.foreach { case (d, node) =>
          if (d >= dist) return result //scalastyle:ignore
          node.nearest(pt, dist) match {
            case some @ Some((d, _)) =>
              dist = d
              result = some
            case None =>
          }
        }
    }
    result
  }

  /**
   * Find the closest `k` entries to `pt` that are within `d0`, and
   * add them to the given priority queue `pq`.
   *
   * This method returns the distance of the farthest entry that is
   * still included.
   */
  def nearestK(pt: Point, k: Int, d0: Double, pq: PriorityQueue[(Double, Entry[A])]): Double = {
    var dist: Double = d0
    this match {
      case Leaf(children, box) =>
        children.foreach { entry =>
          val d = entry.geom.distance(pt)
          if (d < dist) {
            pq += ((d, entry))
            if (pq.size > k) {
              pq.dequeue
              dist = pq.head._1
            }
          }
        }
      case Branch(children, box) =>
        val cs = children.map(node => (node.box.distance(pt), node)).sortBy(_._1)
        cs.foreach { case (d, node) =>
          if (d >= dist) return dist //scalastyle:ignore
          dist = node.nearestK(pt, k, dist, pq)
        }
    }
    dist
  }


  /**
   * Count the number of entries contained within `space`.
   */
  def count(space: Box): Int =
    if (!space.isFinite) 0 else {
      def recur(node: Node[A]): Int = node match {
        case Leaf(children, box) =>
          var n = 0
          var i = 0
          while (i < children.length) {
            if (space.contains(children(i).geom)) n += 1
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
    searchIterator(entry.geom.toBox, _ == entry).nonEmpty

  /**
   * Transform each entry's value using the given `f`, returning a new
   * node.
   */
  def map[B](f: A => B): Node[B] = this match {
    case Leaf(cs, box) =>
      Leaf(cs.map(e => Entry(e.geom, f(e.value))), box)
    case Branch(cs, box) =>
      Branch(cs.map(_.map(f)), box)
  }
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

    if (!box.contains(entry.geom)) None else loop(0)
  }
}


case class Leaf[A](children: Vector[Entry[A]], box: Box) extends Node[A] {

  def remove(entry: Entry[A]): Option[(Joined[Entry[A]], Option[Node[A]])] = {
    if (!box.contains(entry.geom)) return None //scalastyle:ignore
    val i = children.indexOf(entry)
    if (i < 0) {
      None
    } else if (children.length == 1) {
      Some((Joined.empty[Entry[A]], None))
    } else if (children.length == 2) {
      Some((Joined(children(1 - i)), None))
    } else {
      val cs = children.take(i) ++ children.drop(i + 1)
      val b = contract(entry.geom, cs.foldLeft(Box.empty)(_ expand _.geom))
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
case class Entry[A](geom: Geom, value: A) extends HasGeom

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
  def splitter[M <: HasGeom](children: Vector[M]): ((Vector[M], Box), (Vector[M], Box)) = {
    val buf = ArrayBuffer(children: _*)
    val (seed1, seed2) = pickSeeds(buf)

    var box1: Box = seed1.geom.toBox
    var box2: Box = seed2.geom.toBox
    val nodes1 = ArrayBuffer(seed1)
    val nodes2 = ArrayBuffer(seed2)

    def add1(node: M): Unit = { nodes1 += node; box1 = box1.expand(node.geom) }
    def add2(node: M): Unit = { nodes2 += node; box2 = box2.expand(node.geom) }

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
  def pickSeeds[M <: HasGeom](nodes: ArrayBuffer[M]): (M, M) = {

    // find the two geometries that have the most space between them
    // in this particular dimension. the sequence is (lower, upper) points
    def handleDimension(pairs: IndexedSeq[(Float, Float)]): (Float, Int, Int) = {
      val (a0, b0) = pairs(0)
      var amin = a0 // min lower coord
      var amax = a0 // max lower coord
      var bmin = b0 // min upper coord
      var bmax = b0 // max upper coord

      var left = 0
      var right = 0
      var i = 1
      while (i < pairs.length) {
        val (a, b) = pairs(i)
        if (a < amin) { amin = a }
        if (a > amax) { amax = a; right = i }
        if (b > bmax) { bmax = b }
        if (b < bmin) { bmin = b; left = i }
        i += 1
      }

      if (left != right) ((bmin - amax) / (bmax - amin), left, right) else (0.0F, 0, 1)
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

package archery

/**
 * This is a small ADT that we use to avoid building too many
 * intermediate vectors.
 *
 * It allows us to concatenate a whole bunch of vectors or single
 * elements cheaply, and then iterate over them later.
 */
sealed trait Joined[A] extends Iterable[A] {

  def iterator: Iterator[A]

  override def isEmpty: Boolean = false

  def ++(that: Joined[A]): Joined[A] =
    if (that.isEmpty) this else Joined.Concat(this, that)

  override def hashCode(): Int =
    iterator.foldLeft(0x0a704453)((x, y) => x + (y.## * 0xbb012349 + 0x337711af))

  override def equals(that: Any): Boolean =
    that match {
      case that: Joined[_] =>
        val it1 = this.iterator
        val it2 = that.iterator
        while (it1.hasNext && it2.hasNext) {
          if (it1.next != it2.next) return false //scalastyle:off
        }
        it1.hasNext == it2.hasNext
      case _ =>
        false
    }
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


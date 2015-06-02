package archery

/**
 * This is a small ADT that we use to avoid building too many
 * intermediate vectors.
 *
 * It allows us to concatenate a whole bunch of vectors or single
 * elements cheaply, and then iterate over them later.
 */
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


package stream

sealed trait Stream[A] {
  import Stream._

  def map[B](f: A => B): Stream[B] =
    Map(this, f)

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def next[C](stream: Stream[C]): Option[C] =
      stream match {
        case Map(s, fun) => next(s).map(fun)
        case FromIterator(s) =>
          if(s.hasNext) Some(s.next()) else None
      }

    next(this) match {
      case None => z
      case Some(a) => foldLeft(f(z, a))(f)
    }
  }
}
object Stream {
  // Stream algebraic data type
  final case class Map[A,B](source: Stream[A], f: A => B) extends Stream[B]
  final case class FromIterator[A](source: Iterator[A]) extends Stream[A]

  def fromIterator[A](source: Iterator[A]): Stream[A] =
    FromIterator(source)

  // def fromSeq[A](source: Seq[A]): Stream[A]
}

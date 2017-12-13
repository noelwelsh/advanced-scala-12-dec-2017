package stream

import cats.Monad
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._

sealed trait Stream[F[_], A] {
  import Stream._

  def zip[B](that: Stream[F, B]): Stream[F, (A,B)] =
    Zip(this, that)

  def map[B](f: A => B): Stream[F, B] =
    Map(this, f)

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit m: Monad[F]): F[B] = {
    def next[C](stream: Stream[F, C]): F[Option[C]] =
      stream match {
        case Zip(l, r) =>
          (next(l), next(r)).mapN((opt1, opt2) => (opt1, opt2).tupled)
        case Map(s, fun) => next(s).map(opt => opt.map(fun))
        case FromIterator(s) =>
          if(s.hasNext) (s.next().some).pure[F] else none.pure[F]
      }

    next(this).flatMap( option =>
      option match {
        case None => z.pure[F]
        case Some(a) => foldLeft(f(z, a))(f)
      }
    )
  }
}
object Stream {
  // Stream algebraic data type
  final case class Zip[F[_],A,B](left: Stream[F,A], right: Stream[F,B]) extends Stream[F,(A,B)]
  final case class Map[F[_],A,B](source: Stream[F,A], f: A => B) extends Stream[F,B]
  final case class FromIterator[F[_],A](source: Iterator[A]) extends Stream[F,A]

  def fromIterator[F[_],A](source: Iterator[A]): Stream[F,A] =
    FromIterator(source)

  // def fromSeq[A](source: Seq[A]): Stream[A]
}

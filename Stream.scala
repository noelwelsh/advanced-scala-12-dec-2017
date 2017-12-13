package stream

import cats.{Id,Monad,MonadError}
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

  def handleErrorWith[E](f: (E) ⇒ F[A])(implicit me: MonadError[F,E]): Stream[F,A] =
    HandleErrorWith(this, f, me)

  def raiseError[E](e: E)(implicit me: MonadError[F,E]): Stream[F,A] =
    RaiseError(this, e, me)

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit m: Monad[F]): F[B] = {
    def next[G[_],C](stream: Stream[G, C])(implicit m: Monad[G]): Option[G[C]] =
      stream match {
        case zip: Zip[G,a,b] =>
          (next(zip.left), next(zip.right)).mapN((g1, g2) => (g1, g2).tupled)
        case map: Map[G,a,b] =>
          next(map.source).map(g => g.map(map.f))
        case handle: HandleErrorWith[G,a,e] =>
          val source: Stream[G,a] = handle.source
          val f: e => G[a] = handle.f
          val me: MonadError[G,e] = handle.monadError
          next(source).map(g => me.handleErrorWith(g)(f))
        case raise: RaiseError[G,a,e] =>
          next(raise.source).map(g => raise.monadError.raiseError(raise.error))
        case FromIterator(s) =>
          if(s.hasNext) (s.next().pure).some else none
      }

    next(this) match {
      case None => z.pure[F]
      case Some(ga) => ga.flatMap(a => foldLeft(f(z, a))(f))
    }
  }

  def toList(implicit m: Monad[F]): F[List[A]] =
    foldLeft(List.empty[A])((b, a) => a :: b)
}
object Stream {
  // Stream algebraic data type
  final case class Zip[F[_],A,B](left: Stream[F,A], right: Stream[F,B]) extends Stream[F,(A,B)]
  final case class Map[F[_],A,B](source: Stream[F,A], f: A => B) extends Stream[F,B]
  final case class HandleErrorWith[F[_],A,E](source: Stream[F,A], f: E => F[A], monadError: MonadError[F,E]) extends Stream[F,A]
  final case class RaiseError[F[_],A,E](source: Stream[F,A], error: E, monadError: MonadError[F,E]) extends Stream[F,A]
  final case class FromIterator[A](source: Iterator[A]) extends Stream[Id,A]

  def fromIterator[A](source: Iterator[A]): Stream[Id,A] =
    FromIterator(source)

  // def fromSeq[A](source: Seq[A]): Stream[A]
}

import scala.util.Try
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait ErrorHandling[F[_], E] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def recover[A](fa: F[A])(pf: PartialFunction[E,A]): F[A]
  // def pure[A](a: A): F[A]
}

object ErrorHandlingInstances {
  implicit def futureErrorHandling(implicit ec: ExecutionContext): ErrorHandling[Future, Throwable] =
    new ErrorHandling[Future, Throwable]{

      def map[A,B](fa: Future[A])(f: A => B): Future[B] =
        fa.map(f)

      def flatMap[A,B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)

      def recover[A](fa: Future[A])(pf: PartialFunction[Throwable,A]): Future[A] =
        fa.recover(pf)
    }

  implicit val tryErrorHandling: ErrorHandling[Try, Throwable] =
    new ErrorHandling[Try, Throwable]{

      def map[A,B](fa: Try[A])(f: A => B): Try[B] =
        fa.map(f)

      def flatMap[A,B](fa: Try[A])(f: A => Try[B]): Try[B] =
        fa.flatMap(f)

      def recover[A](fa: Try[A])(pf: PartialFunction[Throwable,A]): Try[A] =
        fa.recover(pf)
    }

  implicit def eitherErrorHandling[E]: ErrorHandling[({ type T[A] = Either[E, A]})#T, E] = {
    type EitherE[A] = Either[E,A]
    new ErrorHandling[EitherE, E]{
      def map[A,B](fa: EitherE[A])(f: A => B): EitherE[B] =
        fa.map(f)

      def flatMap[A,B](fa: EitherE[A])(f: A => EitherE[B]): EitherE[B] =
        fa.flatMap(f)

      def recover[A](fa: EitherE[A])(pf: PartialFunction[E,A]): EitherE[A] =
        fa match {
          case Left(e) => pf.lift(e).map(a => Right(a)).getOrElse(Left(e))
          case Right(a) => Right(a)
        }
    }
  }
}

object Examples {
  import ErrorHandlingInstances._
  import ExecutionContext.Implicits.global

  val theFuture = Future.successful(1)
  val theTry = Try(1)

  type Result[A] = Either[String, A]
  val theEither: Result[Int] = Right(1)

  def doSomething[F[_],E](fa: F[Int])(implicit eh: ErrorHandling[F, E]): F[Int] = {
    val stepOne = eh.map(fa)(f => f + 1)
    eh.recover(stepOne){case _ => 42}
  }

  println(doSomething(theFuture))
  println(doSomething(theTry))
  println(doSomething(theEither))
}

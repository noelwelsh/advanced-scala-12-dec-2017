sealed trait FreeMonad[A] {
  import FreeMonad._

  def flatMap[B](f: (A) ⇒ FreeMonad[B]): FreeMonad[B] =
    FlatMap(this, f)
}
object FreeMonad {
  final case class FlatMap[A,B](fa: FreeMonad[A], f: A => FreeMonad[B]) extends FreeMonad[B]
  final case class Pure[A](x: A) extends FreeMonad[A]

  def pure[A](x: A): FreeMonad[A] =
    Pure(x)
}

package calcTagless

import cats.effect.IO
// import cats.instances.all._
// import cats.syntax.all._

// Tagless final style implementation
trait Expr[A] {
  def add(l: A, r: A): A
  def subtract(l: A, r: A): A
  def multiply(l: A, r: A): A
  def divide(l: A, r: A): A
  def literal(number: Double): A
}
object DoubleInterpreter extends Expr[Double] {
  def add(l: Double, r: Double): Double =
    l + r
  def subtract(l: Double, r: Double): Double =
    l - r
  def multiply(l: Double, r: Double): Double =
    l * r
  def divide(l: Double, r: Double): Double =
    l / r
  def literal(number: Double): Double =
    number
}
object StringInterpreter extends Expr[String] {
  def add(l: String, r: String): String =
    s"($l + $r)"
  def subtract(l: String, r: String): String =
    s"($l - $r)"
  def multiply(l: String, r: String): String =
    s"($l * $r)"
  def divide(l: String, r: String): String =
    s"($l / $r)"
  def literal(number: Double): String =
    number.toString
}
object PersistentCalculator extends Expr[IO[Double]] {
  def add(l: IO[Double], r: IO[Double]): IO[Double] =
    for {
      lv <- l
      rv <- r
      r  <- IO { println(s"Evaluating $lv + $rv") }.map(_ => lv + rv)
    } yield r

  def subtract(l: IO[Double], r: IO[Double]): IO[Double] =
    for {
      lv <- l
      rv <- r
      r  <- IO { println(s"Evaluating $lv - $rv") }.map(_ => lv - rv)
    } yield r

  def multiply(l: IO[Double], r: IO[Double]): IO[Double] =
    for {
      lv <- l
      rv <- r
      r  <- IO { println(s"Evaluating $lv * $rv") }.map(_ => lv * rv)
    } yield r

  def divide(l: IO[Double], r: IO[Double]): IO[Double] =
    for {
      lv <- l
      rv <- r
      r  <- IO { println(s"Evaluating $lv / $rv") }.map(_ => lv / rv)
    } yield r

  def literal(number: Double): IO[Double] =
    IO { println(s"Evaluating literal $number") }.map(_ => number)
}
object Example {
  def anExpression[A](expr: Expr[A]): A =
    expr.add(expr.literal(1.0), expr.add(expr.literal(2.0), expr.literal(3.0)))

  println(anExpression(DoubleInterpreter))
  println(anExpression(StringInterpreter))
}

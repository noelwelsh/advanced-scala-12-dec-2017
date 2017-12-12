package calc

sealed trait Expr {
  import Expr._

  def add(l: Expr, r: Expr): Expr =
    Add(l, r)
  def subtract(l: Expr, r: Expr): Expr =
    Subtract(l, r)
  def multiply(l: Expr, r: Expr): Expr =
    Multiply(l, r)
  def divide(l: Expr, r: Expr): Expr =
    Divide(l, r)
  def literal(number: Double): Expr =
    Literal(number)
}
object Expr {
  final case class Add(l: Expr, r: Expr) extends Expr
  final case class Subtract(l: Expr, r: Expr) extends Expr
  final case class Multiply(l: Expr, r: Expr) extends Expr
  final case class Divide(l: Expr, r: Expr) extends Expr
  final case class Literal(number: Double) extends Expr
}
object DoubleInterpreter {
  import Expr._

  def eval(expr: Expr): Double =
    expr match {
      case Add(l, r) => eval(l) + eval(r)
      case Subtract(l, r) => eval(l) - eval(r)
      case Multiply(l, r) => eval(l) * eval(r)
      case Divide(l, r) => eval(l) / eval(r)
      case Literal(n) => n
    }
}

object PrintingInterpreter {
  import Expr._

  def eval(expr: Expr): String =
    expr match {
      case Add(l, r) => s"(${eval(l)} + ${eval(r)})"
      case Subtract(l, r) => s"(${eval(l)} - ${eval(r)})"
      case Multiply(l, r) => s"(${eval(l)} * ${eval(r)})"
      case Divide(l, r) => s"(${eval(l)} / ${eval(r)})"
      case Literal(n) => n.toString
    }
}

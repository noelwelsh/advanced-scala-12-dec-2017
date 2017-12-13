package free

import cats.Id
import cats.free.Free
import cats.arrow.FunctionK

sealed trait DatabaseOps[A]
final case class Read[A](key: Int) extends DatabaseOps[String]
final case class Write[A](key: Int, value: String) extends DatabaseOps[Unit]


object Database {
  type Database[A] = Free[DatabaseOps, A]

  def read(key: Int): Database[String] =
    Free.liftF(Read(key))

  def write(key: Int, value: String): Database[Unit] =
    Free.liftF(Write(key, value))
}

object MapInterpreter extends FunctionK[DatabaseOps, Id] {
  var db: Map[Int, String] = Map.empty

  def apply[A](fa: DatabaseOps[A]): Id[A] =
    fa match {
      case Read(k) => db.apply(k)
      case Write(k, v) => db = db + (k -> v)
    }
}

object Example {
  import Database._

  val program =
    for {
      _ <- write(1, "Hi!")
      v <- read(1)
    } yield v

  def run =
    program.foldMap(MapInterpreter)
}




import scala.concurrent.{ExecutionContext,Future}
import cats.Monoid
import cats.syntax.all._
import cats.instances.all._

object MapReduce {
  val nSplits = 2

  def apply[A,B](data: List[A], f: A => B)(implicit m: Monoid[B], ec: ExecutionContext): Future[B] = {
    val size = data.size
    val groups = data.grouped(size / nSplits)
    groups.map(Future.successful(_).map(_.map(f).combineAll)).toList.sequence.map(_.combineAll)
  }
}

package arguments.naive

trait Argument[+T] { self =>
  def consume(args: Seq[String]): Result[(Seq[String], T)]
  def usage: String
}

trait Reads[I, O] {
  import scala.util.Try
  def read(s: I): Try[O]
}

object Reads {
  import scala.util.Try

  def apply[T](f: String => Try[T]): Reads[String, T] = new Reads[String, T] {
    override def read(s: String): Try[T] = f(s)
  }
}

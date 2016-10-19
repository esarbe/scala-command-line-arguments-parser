package arguments

trait Argument[+T] { self =>
  def consume(args: Seq[String]): Result[(Seq[String], T)]
  def usage: String
}

trait Reads[I, O] {
  import scala.util.Try
  def read(s: I): Try[O]
}

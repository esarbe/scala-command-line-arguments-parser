package arguments

trait Argument[T] { self =>
  def consume(args: Seq[String]): Result[(Seq[String], T)]
  def name: String
}

trait Reads[T] {
  def read(s: String): Result[T]
}

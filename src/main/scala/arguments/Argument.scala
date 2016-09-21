package arguments

trait Argument[T] { self =>
  def consume(args: Seq[String]): Either[String, (Seq[String], T)]
  def name: String
}

trait Reads[T] {
  def read(s: String): Either[String, T]
}

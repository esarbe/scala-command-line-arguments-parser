package arguments

trait Argument[+T] { self =>
  def consume(args: Seq[String]): Result[(Seq[String], T)]
  def name: String
}

trait Reads[I, O] {
  def read(s: I): Result[O]
}

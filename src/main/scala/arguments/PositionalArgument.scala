package arguments

case class PositionalArgument[T](aName: String)(implicit reads: Reads[String, T]) extends Argument[T]{
  override def consume(args: Seq[String]): Result[(Seq[String], T)] = args.toList match {
    case Nil => Left(ArgumentExpected(this))
    case head :: tail => reads.read(head).right.map((tail, _))
  }

  override def usage: String = s"< $aName >"
}
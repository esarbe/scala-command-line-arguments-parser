package arguments.naive

import arguments._

case class PositionalArgument[T](aName: String)(implicit reads: Reads[String, T]) extends Argument[T]{

  import EitherSyntax._
  import TrySyntax._

  override def consume(args: Seq[String]): Result[(Seq[String], T)] = args.toList match {
    case Nil => Left(ArgumentExpected(this))
    case head :: tail => reads.read(head).toEither(CouldNotReadValue(head, _)).map((tail, _))
  }

  override def usage: String = s"< $aName >"
}

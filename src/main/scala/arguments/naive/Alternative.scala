package arguments.naive

import arguments.naive.EitherSyntax._

final case class Alternative[S](alternatives: Argument[S]*) extends Argument[S] {
  override def consume(args: Seq[String]): Result[(Seq[String], S)] = {
    val (errors, results) = alternatives.map(_.consume(args)).segregate

    results.toList match {
      case Nil => Left(MultipleErrors(errors.toList))
      case head :: Nil => Right(head)
      case head :: rest => Left(MutuallyExclusive(alternatives:_*))
    }

  }
  override def usage: String = s"[ ${alternatives.map(_.usage).mkString(" | ")} ]"
}

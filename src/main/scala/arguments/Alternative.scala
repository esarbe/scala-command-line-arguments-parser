package arguments

import EitherUtil._

final case class Alternative[S](alternatives: Argument[S]*) extends Argument[S] {
  override def consume(args: Seq[String]): Result[(Seq[String], S)] = {
    val (errors, results) = alternatives.map(_.consume(args)).segregate

    results.toList match {
      case Nil => Left(MultipleErrors(errors.map(_.left.get)))
      case head :: Nil => head
      case head :: rest => Left(MutuallyExclusive(alternatives:_*))
    }

  }
  override def name: String = s"[ ${alternatives.map(_.name).mkString(" | ")} ]"
}

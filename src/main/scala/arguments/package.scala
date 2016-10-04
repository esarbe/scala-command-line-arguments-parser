package object arguments {

  sealed trait Error
  case class ArgumentExpected(argument: Argument[_]) extends Error
  case class ArgumentPresentMultipleTimes(argument: Argument[_]) extends Error
  case class UnknownArgument[T](argument: String) extends Error
  case class UnknownArguments(arguments: Seq[String]) extends Error
  case class ValueExpected(argument: Argument[_]) extends Error
  case class CouldNotReadValue(argument: Argument[_], maybeException: Option[Throwable]) extends Error
  case class MultipleErrors(errors: Seq[Error]) extends Error
  case class MutuallyExclusive(arguments: Argument[_]*) extends Error

  type Result[A] = Either[Error, A]

}

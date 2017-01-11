package arguments

package object naive {

  sealed trait Error
  case class ArgumentExpected(argument: Argument[_]) extends Error
  case class ArgumentPresentMultipleTimes(argument: Argument[_]) extends Error
  case class UnknownArgument[T](argument: String) extends Error
  case class UnknownArguments(arguments: Seq[String]) extends Error
  case class ValueExpected(argument: Argument[_]) extends Error
  case class CouldNotReadValue[T](value: T, throwable: Throwable) extends Error
  case class MultipleErrors(errors: List[Error]) extends Error
  case class MutuallyExclusive(arguments: Argument[_]*) extends Error
  case class HelpRequested(argument: Argument[_]) extends Error

  type Result[A] = Either[Error, A]

}

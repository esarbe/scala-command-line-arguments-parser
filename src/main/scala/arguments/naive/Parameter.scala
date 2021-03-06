package arguments.naive

import arguments._

final case class Parameter[T](short: Char)(implicit reader: Reads[String, T]) extends Argument[T] {

  import EitherSyntax._
  import TrySyntax._

  val notFound = Left(ArgumentExpected(this))
  val requiresValue = Left(ValueExpected(this))
  val presentMultipleTimes = Left(ArgumentPresentMultipleTimes(this))
  val paramKey = s"-$short"

  override def usage: String = s"-$short"
  override def consume(args: Seq[String]): Result[(Seq[String], T)] = {

    sealed trait State
    case object NotFound extends State
    case object ExpectingValue extends State
    case object DuplicateParameter extends State
    final case class FoundValue(value: String) extends State

    val (rest, state) = args.foldLeft((List[String](), NotFound: State)){ case ((rest, state), curr) =>
      state match {
        case NotFound =>
          if (curr == paramKey) (rest, ExpectingValue)
          else (curr :: rest , NotFound)
        case ExpectingValue => (rest, FoundValue(curr))
        case f @ FoundValue(_) =>
          if (curr == paramKey) (curr :: rest, DuplicateParameter)
          else (curr :: rest, f)
        case dp @ DuplicateParameter => (curr :: rest, dp)
      }
    }

    val result = state match {
      case NotFound => notFound
      case ExpectingValue => requiresValue
      case FoundValue(value) => reader.read(value).toEither(CouldNotReadValue(value, _))
      case DuplicateParameter => presentMultipleTimes
    }

    result.map { value =>
      (rest, value)
    }
  }
}

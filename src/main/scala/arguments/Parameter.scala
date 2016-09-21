package arguments

final case class Parameter[T: Reads](short: Char) extends Argument[T] {
  override def name: String = s"-$short"
  override def consume(args: Seq[String]): Either[String, (Seq[String], T)] = {

    sealed trait State
    case object NotFound extends State
    case object ExpectingValue extends State
    case object DuplicateParameter extends State
    final case class FoundValue(value: String) extends State

    val reader = implicitly[Reads[T]]

    val notFound = Left(s"parameter '$short' is required")
    val requiresValue = Left(s"parameter '$short' requires a value" )
    val paramKey = s"-$short"

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
      case FoundValue(value) => reader.read(value)
      case DuplicateParameter => Left(s"parameter '$short' found more than once")
    }

    result.right.map { value =>
      (rest, value)
    }
  }
}

package arguments

case class Flag[T](c: Char, defaults: (T, T)) extends Argument[T] {

  val (ifFound, ifNotFound) = defaults

  val presentMultipleTimes = Left(ArgumentPresentMultipleTimes(this))

  override def consume(args: Seq[String]): Result[(Seq[String], T)] = {

    sealed trait State
    case object NotFound extends State
    case object Found extends State
    case object DuplicateFlag extends State

    val (rest, state) = args.foldLeft((Seq[String](), NotFound: State)) { case ((rest, state), curr) =>
      state match {
        case NotFound =>
          if (isFlagPresent(curr)) {
            val remaining = stripFlag(curr)
            if (remaining.isEmpty) (rest, Found)
            else (rest :+ remaining, Found)
          } else (args, NotFound)
        case Found =>
          if (isFlagPresent(curr)) (args, DuplicateFlag)
          else (rest :+ curr, Found)
        case DuplicateFlag => (args, DuplicateFlag)
      }
    }

    val value = state match {
      case Found => Right(ifFound)
      case NotFound => Right(ifNotFound)
      case DuplicateFlag => presentMultipleTimes
    }

    value.right.map { (rest, _)}
  }

  override def usage: String = s"[ $c ]"

  private def isFlagPresent(arg: String): Boolean = {
    arg.startsWith("-") && !arg.startsWith("--") && arg.contains(c)
  }

  private def stripFlag(arg: String): String = {
    val remaining = arg.filterNot(_ == c)
    if (remaining == "-") ""
    else remaining
  }
}

package arguments

case class Flag[T: Flags](c: Char) extends Argument[T] {

  sealed trait State
  case object NotFound extends State
  case object Found extends State
  case object DuplicateFlag extends State

  val flagger = implicitly[Flags[T]]

  override def consume(args: Seq[String]): Either[String, (Seq[String], T)] = {
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
          else (args, Found)
        case DuplicateFlag => (args, DuplicateFlag)
      }
    }

    val value = state match {
      case Found => flagger.flags(true)
      case NotFound => flagger.flags(false)
      case DuplicateFlag => Left(s"duplicate flag '$c' present")
    }

    value.right.map { (rest, _)}
  }

  override def name: String = c.toString

  private def isFlagPresent(arg: String): Boolean = {
    arg.startsWith("-") && !arg.startsWith("--") && arg.contains(c)
  }

  private def stripFlag(arg: String): String = {
    val remaining = arg.filterNot(_ == c)
    if (remaining == "-") ""
    else remaining
  }
}

trait Flags[T] {
  def flags(present: Boolean): Either[String, T]
}

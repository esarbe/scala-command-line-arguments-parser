package arguments.naive

class HelpFlag extends Flag[Boolean]('h', (true, false)) {

  override def consume(args: Seq[String]): Result[(Seq[String], Boolean)] = {
    super.consume(args)
      .right.flatMap {  case (_ , found) =>
        if (found) Right((args, found))
        else Left(ArgumentExpected(this))
      }.left.flatMap {
        case _: ArgumentPresentMultipleTimes => Right((args, true))
        case default => Left(default)
      }
  }

  def prioritizeHelp(err: Error): Option[Error] = err match {
    case err: HelpRequested => Some(err)
    case MultipleErrors(head :: tail) => prioritizeHelp(head).orElse(prioritizeHelp(MultipleErrors(tail)))
    case _ => None
  }
}

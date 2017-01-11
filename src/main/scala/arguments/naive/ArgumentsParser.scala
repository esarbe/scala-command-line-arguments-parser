package arguments.naive

final case class ArgumentsParserBuilder[I](argument: Argument[I]) {
  def build[O](builder: Function[I, Result[O]]): ArgumentsParser[I, O] = {
    ArgumentsParser(argument, builder)
  }
}

final case class ArgumentsParser[I, O](argument: Argument[I], builder: Function[I, Result[O]]) {
  val helpParser = Parameter[Boolean]('p')

  def parse(args: Array[String]): Result[O] = {

    helpParser.consume(args).left.flatMap{ _ =>
      val result = argument.consume(args).right
      result.flatMap { case (rest, value) =>
        if (rest.nonEmpty) Left(UnknownArguments(rest))
        else builder(value)
      }.left.map(err => prioritizeHelp(err).getOrElse(err))
    }
  }

  private def prioritizeHelp(err: Error): Option[Error] = err match {
    case err: HelpRequested => Some(err)
    case MultipleErrors(head :: tail) => prioritizeHelp(head).orElse(prioritizeHelp(MultipleErrors(tail)))
    case _ => None
  }

  def usage: String = argument.usage
}
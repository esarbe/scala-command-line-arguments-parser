package arguments.naive

final case class ArgumentsParserBuilder[I](argument: Argument[I]) {
  def build[O](builder: Function[I, Result[O]]): ArgumentsParser[I, O] = {
    ArgumentsParser(argument, builder)
  }
}

final case class ArgumentsParser[I, O](argument: Argument[I], builder: Function[I, Result[O]]) {
  val helpFlag = new HelpFlag()

  def parse(args: Array[String]): Result[O] = {

    helpFlag.consume(args).right
      .flatMap(_=> Left(HelpRequested(argument)))
      .left.flatMap { _ =>
        val result = argument.consume(args).right
        result.flatMap { case (rest, value) =>
          if (rest.nonEmpty) Left(UnknownArguments(rest))
          else builder(value)
      }.left.map(err => helpFlag.prioritizeHelp(err).getOrElse(err))
    }
  }

  def usage: String = argument.usage
}
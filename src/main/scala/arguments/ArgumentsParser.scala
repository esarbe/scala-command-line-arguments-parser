package arguments

final case class ArgumentsParserBuilder[I](argument: Argument[I]) {
  def build[O](builder: Function[I, Result[O]]): ArgumentsParser[I, O] = {
    ArgumentsParser(argument, builder)
  }
}

final case class ArgumentsParser[I, O](argument: Argument[I], builder: Function[I, Result[O]]) {
  def parse(args: Array[String]): Result[O] = {
    val result = argument.consume(args).right
    result.flatMap { case (rest, value) =>
      if (!rest.isEmpty) Left(UnknownArguments(rest))
      else builder(value)
    }
  }
}


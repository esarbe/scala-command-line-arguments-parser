package arguments

final case class ArgumentsParser[I, O](argument: Argument[I], builder: I => Either[String, O]) {
  def parse(args: Array[String]): Either[String, O] = {
    val result = argument.consume(args).right
    result.flatMap { case (rest, value) =>
      if (!rest.isEmpty) Left(s"could not consume arguments: ${rest.mkString(" ")}")
      else builder(value)
    }
  }
}


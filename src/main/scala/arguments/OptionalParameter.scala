package arguments

case class OptionalParameter[T](parameter: Parameter[T]) extends Argument[Option[T]] {
  override def consume(args: Seq[String]): Result[(Seq[String], Option[T])] = {

    val optionalResult = parameter.consume(args).right.flatMap {
      case (rest: Seq[String], value: T) => Right(rest, Some(value))
    }.left.flatMap {
      case ArgumentExpected(a) => Right((args, None))
      case e: Error => Left(e)
    }

    optionalResult
  }

  override def name: String = s"[ ${parameter.name} ]"
}
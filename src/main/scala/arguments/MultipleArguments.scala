package arguments

case class MultipleArguments[A, B](first: Argument[A], second: Argument[B]) extends Argument[(A, B)] {
  override def consume(args: Seq[String]): Result[(Seq[String], (A, B))] = {
    first.consume(args).right.flatMap { case (firstRemainingArgs, firstResult) =>
      second.consume(firstRemainingArgs).right.map { case (secondRemainingArgs, secondResult) =>
        (secondRemainingArgs, (firstResult, secondResult))
      }
    }
  }

  override def name: String = first.name + ", " + second.name
}
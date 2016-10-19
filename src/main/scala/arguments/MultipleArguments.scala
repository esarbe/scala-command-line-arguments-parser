package arguments


case class MultipleArguments[A, B](first: Argument[A], second: Argument[B]) extends Argument[(A, B)] {

  import EitherSyntax._

  override def consume(args: Seq[String]): Result[(Seq[String], (A, B))] = {
    first.consume(args).right.flatMap { case (firstRemainingArgs, firstResult) =>
      second.consume(firstRemainingArgs).map { case (secondRemainingArgs, secondResult) =>
        (secondRemainingArgs, (firstResult, secondResult))
      }
    }
  }

  override def usage: String = first.usage + ", " + second.usage
}
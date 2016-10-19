package arguments

object EitherSyntax {
  implicit def eitherOps[A, B](eithers: Seq[Either[A, B]]): EitherOps[A, B] = new EitherOps(eithers)
}

class EitherOps[A, B](val eithers: Seq[Either[A, B]]) extends AnyVal {

  import scala.collection.generic.CanBuildFrom

  def segregate(
    implicit
      cbfa: CanBuildFrom[Seq[Left[A, B]], Left[A, B], Seq[Left[A, B]]],
      cbfb: CanBuildFrom[Seq[Right[A, B]], Right[A, B], Seq[Right[A, B]]]
  ): (Seq[Left[A, B]], Seq[Right[A, B]]) = {

    val aBuilder = cbfa()
    val bBuilder = cbfb()

    eithers.foreach {
      case l@Left(left) => aBuilder += l
      case r@Right(right) => bBuilder += r
    }

    (aBuilder.result(), bBuilder.result())
  }
}


package arguments

object EitherSyntax {
  implicit def eitherOps[A, B](eithers: Seq[Either[A, B]]): EitherOps[A, B] = new EitherOps(eithers)
}

class EitherOps[A, B](val eithers: Seq[Either[A, B]]) extends AnyVal {

  import scala.collection.generic.CanBuildFrom

  def segregate(
    implicit
      cbfa: CanBuildFrom[Seq[_], A, Seq[A]],
      cbfb: CanBuildFrom[Seq[_], B, Seq[B]]
  ): (Seq[A], Seq[B]) = {

    val aBuilder = cbfa()
    val bBuilder = cbfb()

    eithers.foreach {
      case l@Left(left) => aBuilder += left
      case r@Right(right) => bBuilder += right
    }

    (aBuilder.result(), bBuilder.result())
  }
}


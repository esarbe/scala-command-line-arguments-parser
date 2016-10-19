package arguments

object EitherSyntax {
  implicit def toEithersOps[A, B](eithers: Seq[Either[A, B]]): EithersOps[A, B] = new EithersOps(eithers)
  implicit def toEitherOps[A, B](either: Either[A, B]): EitherOps[A, B] = new EitherOps(either)
}

class EitherOps[A, B](val either: Either[A, B]) extends AnyVal {
  def map[C](f: B => C): Either[A, C] = either match {
    case Right(right) => Right(f(right))
    case Left(left) => Left(left)
  }
}

class EithersOps[A, B](val eithers: Seq[Either[A, B]]) extends AnyVal {

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


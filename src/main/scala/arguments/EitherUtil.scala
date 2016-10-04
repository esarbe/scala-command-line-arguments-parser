package arguments

import scala.collection.generic.CanBuildFrom

object EitherUtil {

  implicit class EitherOps[A, B](eithers: Seq[Either[A, B]])(
    implicit
      cbfa: CanBuildFrom[Seq[Left[A, B]], Left[A, B], Seq[Left[A, B]]],
      cbfb: CanBuildFrom[Seq[Right[A, B]], Right[A, B], Seq[Right[A, B]]]
  ) {

    def segregate: (Seq[Left[A, B]], Seq[Right[A, B]]) = {

      val aBuilder = cbfa()
      val bBuilder = cbfb()

      eithers.foreach {
        case l @ Left(left) => aBuilder += l
        case r @ Right(right) => bBuilder += r
      }

      (aBuilder.result(), bBuilder.result())
    }
  }
}

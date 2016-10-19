package arguments

import scala.util.{Failure, Success, Try}

object TrySyntax {
  implicit def toTryOps[T](tried: Try[T]): TryOps[T] = new TryOps(tried)
}

class TryOps[T](val tried: Try[T]) extends AnyVal{
  def toEither[S](recover: Throwable => S): Either[S, T] = tried match {
    case Success(value) => Right(value)
    case Failure(thrown) => Left(recover(thrown))
  }
}

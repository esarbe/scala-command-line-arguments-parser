package arguments

final case class Alternative[S: Reads](left: Argument[S], right: Argument[S]) extends Argument[S] {
  override def consume(args: Seq[String]): Result[(Seq[String], S)] = {
    (left.consume(args), right.consume(args)) match {
      case (a@ Left(_), b@ Right(_)) => b
      case (a@ Right(_), b@ Left(_)) => a
      case (a@ Left(aError), b@ Left(bError)) => Left(MultipleErrors(aError, bError))
      case (a@ Right(_), b@ Right(_)) => Left(MutuallyExclusive(left, right))
    }
  }
  override def name: String = s"${left.name} | ${right.name}"
}

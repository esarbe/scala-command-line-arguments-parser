package arguments

final case class Alternative[S: Reads](left: Argument[S], right: Argument[S]) extends Argument[S] {
  override def consume(args: Seq[String]): Either[String, (Seq[String], S)] = {
    (left.consume(args), right.consume(args)) match {
      case (a@ Left(_), b@ Right(_)) => b
      case (a@ Right(_), b@ Left(_)) => a
      case (a@ Left(aMessage), b@ Left(bMessage)) => Left(s"$aMessage, $bMessage")
      case (a@ Right(_), b@ Right(_)) => Left(s"arguments ${left.name} and ${right.name} are mutually exclusive")
    }
  }
  override def name: String = s"${left.name} | ${right.name}"
}

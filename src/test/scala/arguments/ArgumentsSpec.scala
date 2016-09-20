package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  type Builder[-I, +O] = I => Either[String, O]
  val noopBuilder: Builder[Any, Any] = in => Right(in)

  trait Reads[T] {
    def read(s: String): Either[String, T]
  }

  implicit val StringReader: Reads[String] = new Reads[String] {
    override def read(s: String): Either[String, String] = Right(s)
  }

  sealed trait Argument[T] { self =>
    def consume(args: Seq[String]): Either[String, (Seq[String], T)]
    def name: String
  }

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

  final case class Parameter[T: Reads](short: Char) extends Argument[T] {
    override def name: String = s"-$short"
    override def consume(args: Seq[String]): Either[String, (Seq[String], T)] = {

      sealed trait State
      case object NotFound extends State
      case object ExpectingValue extends State
      case object DuplicateParameter extends State
      case class FoundValue(value: String) extends State

      val reader = implicitly[Reads[T]]

      val notFound = Left(s"parameter '$short' is required")
      val requiresValue = Left(s"parameter '$short' requires a value" )
      val paramKey = s"-$short"

      val (rest, state) = args.foldLeft((List[String](), NotFound: State)){ case ((rest, state), curr) =>
          state match {
            case NotFound =>
              if (curr == paramKey) (rest, ExpectingValue)
              else (curr :: rest , NotFound)
            case ExpectingValue => (rest, FoundValue(curr))
            case f @ FoundValue(_) =>
              if (curr == paramKey) (curr :: rest, DuplicateParameter)
              else (curr :: rest, f)
            case dp @ DuplicateParameter => (curr :: rest, dp)
          }
      }
      val result = state match {
        case NotFound => notFound
        case ExpectingValue => requiresValue
        case FoundValue(value) => reader.read(value)
        case DuplicateParameter => Left(s"parameter '$short' found more than once")
      }

      result.right.map { value =>
        (rest, value)
      }
    }
  }

  final case class ArgumentsParser[I <: Any, O <: Any](argument: Argument[I], builder: I => Either[String, O]) {
    def parse(args: Array[String]): Either[String, O] = {
      val result = argument.consume(args).right
      result.flatMap { case (rest, value) =>
        if (!rest.isEmpty) Left(s"could not consume arguments: ${rest.mkString("")}")
        else builder(value)
      }
    }
  }

  test("expecting a single parameter") {
    val parser = ArgumentsParser(
      Parameter[String]('p'),
      builder = (s: String) => Right(s)
    )

    assert(parser.parse(Array("-p", "set")) == Right("set"))
    assert(parser.parse(Array("-o")) isLeft)
    assert(parser.parse(Array("-p")) isLeft)
    assert(parser.parse(Array("")) isLeft)
    assert(parser.parse(Array("-f", "-p", "set")) isLeft)
    assert(parser.parse(Array("-p", "foo", "-p")) isLeft)
  }

  test("expecting one of two mutually exclusive arguments") {
    val parser = ArgumentsParser(
      Alternative(
        Parameter[String]('p'),
        Parameter[String]('q')
      ),
      builder = (s: String) => Right(s)
    )

    assert(parser.parse(Array("-p", "bar")) == Right("bar"))
    assert(parser.parse(Array("-q", "foo")) == Right("foo"))
    assert(parser.parse(Array("-q", "foo", "-p", "bar")) isLeft)
    assert(parser.parse(Array("-q", "foo", "bar")) isLeft)
  }


}

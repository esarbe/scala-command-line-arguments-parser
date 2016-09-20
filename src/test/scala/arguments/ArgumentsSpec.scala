package arguments

import org.scalatest.FunSuite

import scala.collection.{GenSeq, GenSeqLike}

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
  }

  final case class Alternative[S: Reads](left: Argument[S], right: Argument[S]) extends Argument[S] {
    override def consume(args: Seq[String]): Either[String, (Seq[String], S)]  = ???
  }

  final case class Parameter[T: Reads](short: Char) extends Argument[T] {
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



  implicit class OptionIndexGenSeq[+A, +Repr](genSeq: Array[A]) {
    def oIndexOf[B >: A](elem: B): Option[Int] = genSeq.indexOf(elem) match {
      case -1 => None
      case a => Some(a)
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


}

package arguments

import org.scalatest.FunSuite

import scala.collection.{GenSeq, GenSeqLike}

class ArgumentsSuite extends FunSuite {

  type Builder[-I, +O] = I => Either[String, O]
  val noopBuilder: Builder[Any, Any] = in => Right(in)

  trait Reads[T] {
    def read(s: String): T
  }

  implicit val StringReader: Reads[String] = new Reads[String] {
    override def read(s: String): String = s
  }

  sealed trait Argument[T] { self =>
    def consume(args: Array[String]): Either[String, T]
  }

  final case class Parameter[T: Reads](short: Char) extends Argument[T] {
    override def consume(args: Array[String]): Either[String, T] = {
      val reader = implicitly[Reads[T]]
      args.oIndexOf(s"-$short")
          .map ( index => Right(index))
          .getOrElse(Left(s"parameter '$short' is required"))
          .right
          .flatMap { index =>
            if (args.isDefinedAt(index + 1)) Right(args(index + 1))
            else Left(s"parameter '$short' requires a value" )
          }
          .right
          .map( value => reader.read(value))

    }
  }

  final case class ArgumentsParser[I <: Any, O <: Any](argument: Argument[I], builder: I => O) {
    def parse(args: Array[String]): Either[String, O] = {
      argument.consume(args).right.map(builder)
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
      builder = (s: String) => s
    )

    assert(parser.parse(Array("-p", "set")) == Right("set"))
    assert(parser.parse(Array("-o")) isLeft)
    assert(parser.parse(Array("-p")) isLeft)
    assert(parser.parse(Array("")) isLeft)
    assert(parser.parse(Array("-f", "-p", "set")) isLeft)
    assert(parser.parse(Array("-p", "foo", "-p")) isLeft)
  }


}

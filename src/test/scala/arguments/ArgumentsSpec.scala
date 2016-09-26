package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  implicit val stringReads: Reads[String] = new Reads[String] {
    override def read(s: String): Either[String, String] = Right(s)
  }

  implicit val booleanFlagger: Flags[Boolean] = new Flags[Boolean] {
    override def flags(present: Boolean): Either[String, Boolean] = Right(present)
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

  test("expecting flag to be present or not") {

    val parser = ArgumentsParser (
      Flag[Boolean]('f'),
      builder = (b: Boolean) => Right(b)
    )

    assert(parser.parse(Array("-f")) == Right(true))
    assert(parser.parse(Array()) == Right(false))
    assert(parser.parse(Array("-g")) isLeft)
  }

  case class MultipleArguments2[A, B](first: Argument[A], second: Argument[B]) extends Argument[(A, B)] {
    override def consume(args: Seq[String]): Either[String, (Seq[String], (A, B))] = {
      first.consume(args).right.flatMap { case (firstRemainingArgs, firstResult) =>
        second.consume(firstRemainingArgs).right.map { case (secondRemainingArgs, secondResult) =>
          (secondRemainingArgs, (firstResult, secondResult))
        }
      }
    }

    override def name: String = first.name + ", " + second.name
  }

  test("expecting flag a be present or not and an parameter to be set") {
    val parser = ArgumentsParserBuilder (
      MultipleArguments2(
        Flag[Boolean]('f'),
        Parameter[String]('p')
      )
    ).build { case result =>
      Right(result)
    }

    assert(parser.parse(Array("-f", "-p", "foo")) == Right((true, "foo")))
    assert(parser.parse(Array("-p", "foo")) == Right((false, "foo")))
    assert(parser.parse(Array()) isLeft)
    assert(parser.parse(Array("-qux")) isLeft)
  }
}

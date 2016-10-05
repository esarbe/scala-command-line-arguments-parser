package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  implicit val stringReads: Reads[String] = new Reads[String] {
    override def read(s: String): Result[String] = Right(s)
  }

  implicit val booleanFlagger: Flags[Boolean] = new Flags[Boolean] {
    override def flags(present: Boolean): Result[Boolean] = Right(present)
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

  test("expecting flag a be present or not and an parameter to be set") {
    val parser = ArgumentsParserBuilder (
      MultipleArguments(
        Flag[Boolean]('f'),
        Parameter[String]('p')
      )
    ).build { Right(_) }

    assert(parser.parse(Array("-f", "-p", "foo")) == Right((true, "foo")))
    assert(parser.parse(Array("-p", "foo")) == Right((false, "foo")))
    assert(parser.parse(Array()) isLeft)
    assert(parser.parse(Array("-qux")) isLeft)
  }

  test("expecting a parameter to be present or not"){
    val parser = ArgumentsParserBuilder (
      OptionalParameter(Parameter[String]('v'))
    ).build(Right(_))

    assert(parser.parse(Array("-f")) isLeft)
    assert(parser.parse(Array("-v")) isLeft)
    assert(parser.parse(Array("-v", "foo", "-g")) isLeft)
    assert(parser.parse(Array("-v", "foo")) == Right(Some("foo")))
    assert(parser.parse(Array()) == Right(None))
  }
}
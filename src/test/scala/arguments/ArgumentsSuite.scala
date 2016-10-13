package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  implicit val stringReads: Reads[String, String] = new Reads[String, String] {
    override def read(s: String): Result[String] = Right(s)
  }

  implicit val int2IntReads: Reads[String, Int] = new Reads[String, Int] {
    import scala.util.{Try, Success, Failure}

    override def read(s: String): Result[Int] = Try(s.toInt) match {
      case Success(value) => Right(value)
      case Failure(thrown) => Left(CouldNotReadValue(s, thrown))
    }
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


  test("expecting one of two commands") {
    trait Action
    case object FooAction extends Action
    case object BarAction extends Action

    val barCommand = Command.build("bar", NoArgument)( (Unit) => BarAction)
    val fooCommand = Command.build("foo", NoArgument)( (Unit) => FooAction)

    val parser = ArgumentsParserBuilder(
      Alternative(
        fooCommand,
        barCommand
      )
    ).build(Right(_))

    assert(parser.parse(Array("foo")) === Right(FooAction))
    assert(parser.parse(Array("bar")) === Right(BarAction))
    assert(parser.parse(Array("foo", "bar")) === Left(MutuallyExclusive(fooCommand, barCommand)))
    assert(parser.parse(Array("-foo")) isLeft)
  }

  test("expecting command and a flag") {

    implicit val reader: Reads[Boolean, String] = new Reads[Boolean, String] {
      override def read(s: Boolean): Result[String] =
        if (s) Right("flag")
        else Right("no-flag")
    }

    val parser = ArgumentsParserBuilder(
      Command("command", Flag[Boolean]('f'))
    ).build(Right(_))

    assert(parser.parse(Array("command")) == Right("no-flag"))
    assert(parser.parse(Array("command", "-f")) == Right("flag"))
    assert(parser.parse(Array("command", "-g")) isLeft)
    assert(parser.parse(Array()) isLeft)

  }

  test("expecting a positional argument") {

    case class PositionalArgument[T](aName: String)(implicit reads: Reads[String, T]) extends Argument[T]{
      override def consume(args: Seq[String]): Result[(Seq[String], T)] = args.toList match {
        case Nil => Left(ArgumentExpected(this))
        case head :: tail => reads.read(head).right.map((tail, _))
      }

      override def usage: String = s"< $aName >"
    }

    val parser = ArgumentsParserBuilder(
      MultipleArguments(
        PositionalArgument[Int]("first"),
        PositionalArgument[Int]("second")
      )
    ).build(Right(_))

    assert(parser.parse(Array("bar")) isLeft)
    assert(parser.parse(Array("1")) isLeft)
    assert(parser.parse(Array("1", "goo")) isLeft)
    assert(parser.parse(Array("1", "2")) === Right((1,2)))
  }
}

package arguments.naive

import arguments.TrySyntax
import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {
  import scala.util.Try

  implicit val stringReads: Reads[String, String] = Reads(util.Success(_))
  implicit val intReads: Reads[String, Int] = Reads(s => Try(s.toInt))
  implicit val doubleReads: Reads[String, Double] = Reads(s => Try(s.toDouble))

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

    val parser = ArgumentsParserBuilder (
      Flag('f')
    ).build( Right(_))

    assert(parser.parse(Array("-f")) == Right(true))
    assert(parser.parse(Array()) == Right(false))
    assert(parser.parse(Array("-g")) isLeft)
  }

  test("expecting flag a be present or not and an parameter to be set") {
    val parser = ArgumentsParserBuilder (
      MultipleArguments(
        Flag('f'),
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
      import scala.util.Success

      override def read(s: Boolean): Try[String] =
        if (s) Success("flag")
        else Success("no-flag")
    }

    val parser = ArgumentsParserBuilder(
      Command("command", Flag('f'))
    ).build(Right(_))

    assert(parser.parse(Array("command")) == Right("no-flag"))
    assert(parser.parse(Array("command", "-f")) == Right("flag"))
    assert(parser.parse(Array("command", "-g")) isLeft)
    assert(parser.parse(Array()) isLeft)
  }

  test("expecting two positional arguments") {

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
    assert(parser.parse(Array("1", "goo", "foo")) isLeft)
  }

  test("expecting a optional parameter and a positional arguments") {

    val parser = ArgumentsParserBuilder(
      MultipleArguments(
        OptionalParameter(Parameter[String]('f')),
        PositionalArgument[Int]("second")
      )
    ).build(Right(_))

    assert(parser.parse(Array("bar")) isLeft)
    assert(parser.parse(Array("1")) === Right((None, 1)))
    assert(parser.parse(Array("1", "-f", "bar")) === Right((Some("bar"), 1)))

    assert(parser.parse(Array("-f", "bar")) isLeft)
    assert(parser.parse(Array("1", "goo", "foo")) isLeft)
  }

  case class TrailingArgument[T](name: String)(implicit reads: Reads[String, T]) extends Argument[Seq[T]] {
    import arguments.EitherSyntax._
    import TrySyntax._

    override def consume(args: Seq[String]): Result[(Seq[String], Seq[T])] = {
      val foo = args.map(r => reads.read(r).toEither(CouldNotReadValue(r, _)))

      val (errors, successes) = foo.segregate

      if (errors.isEmpty) Right((Seq.empty[String], successes))
      else Left(MultipleErrors(errors.toList))
    }

    override def usage: String = "$name..."
  }

  test("expecting a trailing argument of type int") {
    val parser = ArgumentsParserBuilder(
      TrailingArgument[Int]("numbers")
    ).build(Right(_))

    assert(parser.parse(Array("1", "2", "3")) === Right(Seq(1,2,3)))
    assert(parser.parse(Array("foo", "qux", "bar")) isLeft)
    assert(parser.parse(Array("-1", "2", "bar")) isLeft)
  }
  test("expecting a command with two positional and trailing arguments or a command with trailing arguments") {
    trait Action
    case class FooAction(tuple: (Double, Double), list: List[Int]) extends Action
    case class BarAction(names: String*)


    val parser = ArgumentsParserBuilder(
      Alternative(
        Command.build("foo", MultipleArguments(
          PositionalArgument[Double]("first"),
          MultipleArguments(
            PositionalArgument[Double]("second"),
            TrailingArgument[Int]("numbers")
          )
        )){ case (first, (second, list)) => FooAction((first, second), list.toList)},

        Command.build("bar", TrailingArgument[String]("names")){ case names => BarAction(names:_*)}

      )
    ).build { Right(_) }

    assert(parser.parse(Array("foo", "1.0d", "2.0d", "1", "2", "3", "4")) === Right(FooAction((1.0, 2.0), List(1,2,3,4))))
    assert(parser.parse(Array("bar", "ay", "bee", "cee", "dee")) === Right(BarAction("ay", "bee", "cee", "dee")))
    assert(parser.parse(Array("qux", "1.0d", "2.0d", "1", "2", "3", "4")) isLeft)
    assert(parser.parse(Array("foo", "1.0d", "2.0d", "bar", "ay", "bee")) isLeft)
  }

  test("help flag") {
    val parser = ArgumentsParserBuilder(
      NoArgument
    ).build { Right(_)}


    println(parser.parse(Array("-h")))
    assert(parser.parse(Array("foo")) isLeft)
    assert(parser.parse(Array("-f")) isLeft)
    assert(parser.parse(Array("-h")) === Left(HelpRequested(NoArgument)))
  }

  test("with help") {
    trait Action
    case object FooAction extends Action
    case object BarAction extends Action

    val fooCommand = Command.build("foo", NoArgument){ (Unit) => FooAction}
    val barCommand = Command.build("bar", NoArgument){ (Unit) => BarAction}
    val fooCommandOrBarCommand = Alternative( barCommand, fooCommand )

    val parser = ArgumentsParserBuilder(
      fooCommandOrBarCommand
    ).build { Right(_) }

    assert(parser.parse(Array("-h","foo", "-h")) === Left(HelpRequested(fooCommandOrBarCommand)))
    assert(parser.parse(Array("foo", "-h")) === Left(HelpRequested(fooCommand)))
    assert(parser.parse(Array("bar", "ay", "bee", "cee", "dee")) === Right(BarAction))
    assert(parser.parse(Array("qux", "1.0d", "2.0d", "1", "2", "3", "4")) isLeft)
  }
}

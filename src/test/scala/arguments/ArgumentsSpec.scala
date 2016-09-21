package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  implicit val stringReads: Reads[String] = new Reads[String] {
    override def read(s: String): Either[String, String] = Right(s)
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

  implicit val booleanFlagger: Flags[Boolean] = new Flags[Boolean] {
    override def flags(present: Boolean): Either[String, Boolean] = Right(present)
  }

  case class Flag[T: Flags](c: Char) extends Argument[T] {

    sealed trait State
    case object NotFound extends State
    case object Found extends State
    case object DuplicateFlag extends State

    val flagger = implicitly[Flags[T]]

    override def consume(args: Seq[String]): Either[String, (Seq[String], T)] = {
      val (rest, state) = args.foldLeft((Seq[String](), NotFound: State)) { case ((rest, state), curr) =>
        state match {
          case NotFound =>
            if (isFlagPresent(curr)) {
              val remaining = stripFlag(curr)
              if (remaining.isEmpty) (rest, Found)
              else (rest :+ remaining, Found)
            } else (args, NotFound)
          case Found =>
            if (isFlagPresent(curr)) (args, DuplicateFlag)
            else (args, Found)
          case DuplicateFlag => (args, DuplicateFlag)
        }
      }

      val value = state match {
        case Found => flagger.flags(true)
        case NotFound => flagger.flags(false)
        case DuplicateFlag => Left(s"duplicate flag '$c' present")
      }

      value.right.map { (rest, _)}
    }

    override def name: String = c.toString

    private def isFlagPresent(arg: String): Boolean = {
      arg.startsWith("-") && !arg.startsWith("--") && arg.contains(c)
    }

    private def stripFlag(arg: String): String = {
      val remaining = arg.filterNot(_ == c)
      if (remaining == "-") ""
      else remaining
    }
  }

  trait Flags[T] {
    def flags(present: Boolean): Either[String, T]
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
}

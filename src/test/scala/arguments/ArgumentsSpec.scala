package arguments

import org.scalatest.FunSuite

class ArgumentsSuite extends FunSuite {

  trait Argument[T]
  case class Parameter[T](short: Char, maybeLong: Option[String]) extends Argument[T]
  case class ArgumentsParser[O](argument: Argument[O]) {
    def parse[T](args: Array[String]): T = ???
  }

  test("expecting a single parameter") {
    val parser = ArgumentsParser(
      Parameter[String]('p', Some("parameter"))
    )

    assert(parser.parse(Array("-p", "set")) == "set")



  }
}

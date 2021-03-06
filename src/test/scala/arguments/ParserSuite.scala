package arguments

import org.scalatest.FunSuite


class ParserSuite extends FunSuite {

  type Parser[A] = String => List[(A, String)]

  object + {
    def unapply[A](xs: String): Option[(Char, String)] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

  implicit class ParserOps[A](p: Parser[A]) {
    def flatMap[B](f: A => Parser[B]) = {
      input: String =>
        val fTupled = (f(_: A)(_: String)).tupled
        p(input).flatMap(fTupled)
    }

    def ap[B](af: Parser[A => B]): Parser[B] = { input =>
      val foo = af(input)

      foo.flatMap { case (f, v) =>
        val pvs = p(v)
        pvs.map { case (pf, pv) =>
          (f(pf), pv)
        }
      }
    }

    def map[B](f: A => B): Parser[B] = {
      flatMap(a => result(f(a)))
    }
  }

  def result[A](a: A): Parser[A] = {
    input: String => List((a, input))
  }

  def zero: Parser[Nothing] = {
    _: String => Nil
  }

  def item: Parser[Char] = {
    case "" => Nil
    case x + xs => List((x, xs))
  }

  def sat(p: Char => Boolean): Parser[Char] = {
    item.flatMap { x =>
      if (p(x)) result(x)
      else zero
    }
  }

  def char(c: Char): Parser[Char] = sat( _ == c )

  def digit: Parser[Char] = sat( _.isDigit )
  def upper: Parser[Char] = sat( _.isUpper )
  def lower: Parser[Char] = sat( _.isLower )
  def whitespace: Parser[Char] = sat(c => c.isWhitespace || c.isSpaceChar)

  def plus[A](u: Parser[A], v: Parser[A]): Parser[A] = { input: String =>
    u(input) ++ v(input)
  }

  def lowers =
    lower.flatMap { c0 =>
      lower.flatMap { c1 =>
        lower.flatMap { c2 =>
          result(List(c0, c1, c2).mkString)
        }
      }
    }

  def letter = plus(lower, upper)
  def alphanum = plus(letter, digit)
  def string(s: String): Parser[String] = s match {
    case "" => result("")
    case x + xs =>
      char(x).flatMap { c =>
        string(xs).flatMap { s =>
          result(c + s)
        }
      }
  }

  def many[T](p: Parser[T]): Parser[List[T]] = {
    plus(p.flatMap { x =>
      many(p).map { xs =>
        x :: xs
      }
    }, result(Nil))
  }

  def many1[T](p: Parser[T]): Parser[List[T]] = {
    p.flatMap { x =>
      many(p).map { xs =>
        x :: xs
      }
    }
  }

  def word2 = many(letter).map { _.mkString }

  def word: Parser[String] = {
    val nonEmptyWord =
      letter.flatMap { x =>
        word.flatMap { xs =>
          result(x + xs)
        }
      }

    plus(nonEmptyWord, result(""))
  }

  test("""sat(x = 'x')("x")"""){
    assert(sat(_ == 'x')("x") === List(('x', "")))
  }

  test("""char('c')("c")"""") {
    assert(char('c')("c") === List(('c', "")))
  }

  test("parser combinator") {
    def twoLowers =
      lower.flatMap { c0 =>
        lower.flatMap { c1 =>
          result(List(c0, c1).mkString)
        }
      }

    twoLowers("abcd") === List(("ab", "cd"))
  }

  test("plus") {
    val ul = plus(upper, lower)

    assert(ul("Abc") === List(('A', "bc")))
    assert(ul("abc") === List(('a', "bc")))
  }

  test("a word") {
    assert(word("one two three").toSet === Set(("one", " two three"), ("on", "e two three"), ("o", "ne two three"), ("","one two three")))
  }

  test("another word") {
    assert(word2("one two three").toSet === Set(("one", " two three"), ("on", "e two three"), ("o", "ne two three"), ("","one two three")))
  }

  test("many parser") {
    val p = many(char('a'))

    assert(p("a").toSet === Set((List('a'), ""), (Nil, "a")))
  }

  test("many1 parser") {
    val p = many1(char('a'))

    assert(p("a").toSet === Set((List('a'), "")))
  }

  test("a string") {
    assert(string("foo")("foo bar baz") === List(("foo", " bar baz")))
  }

  test("mandatory argument") {
    def argument(long: String, short: Char, description: String) = {}
  }
}

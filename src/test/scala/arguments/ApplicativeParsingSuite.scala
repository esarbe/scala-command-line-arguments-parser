package arguments

import org.scalatest.FunSuite

/**
  * applicative parsing according to
  * Deterministic, Error-Correcting Combinator Parsers by S. Doaitse Swierstra and Luc Duponcheel
  */
class ApplicativeParsingSuite extends FunSuite {
  trait Symbol[T]
  implicit val stringSymbol: Symbol[String] = new Symbol[String] {}
  implicit val charSymbol: Symbol[Char] = new Symbol[Char] {}

  object algebra {

    trait Parser[P[_, _]] {
      def empty[A, S: Symbol](a: => A): P[S, A]
      def symbol[S: Symbol](s: => S): P[S, S]

      def alt[A, S: Symbol](p: => P[S, A])(a: => P[S, A]): P[S, A]
      def seq[A, B, S: Symbol](f: => P[S, A => B])(p: => P[S, A]): P[S, B]
      def err[A, S: Symbol](p: => P[S, A])(a: => A, s: => String): P[S, A]
    }
  }

  object dsl {
    import algebra._

    implicit class ParserOps[S: Symbol, A, P[_, _]: Parser](parser: P[S, A]) {
      def empty[E](a: E): P[S, E] = implicitly[Parser[P]].empty(a)
      def symbol(s: S): P[S, S] = implicitly[Parser[P]].symbol(s)
      def alt(a: => P[S, A]): P[S, A] = implicitly[Parser[P]].alt(parser)(a)

      def err(a: => A, s: => String): P[S, A] = implicitly[Parser[P]].err(parser)(a, s)
      def <|>(a: => P[S, A]): P[S, A] = alt(a)

      def map[B](f: A => B): P[S, B] = implicitly[Parser[P]].empty[A => B, S](f) <*> parser
      def |?|(a: => A, s: => String): P[S, A] = err(a, s)
      def or(a: => A): P[S, A] = parser <|> parser.empty(a)
    }

    implicit class ParserLiftedFunctionOps[S: Symbol, A, B, P[_, _]: Parser](parser: P[S, A => B]) {
      def seq(p: => P[S, A]): P[S, B] = implicitly[Parser[P]].seq(parser)(p)
      def <*>(p: => P[S, A]): P[S, B] = seq(p)
    }

    def many[S: Symbol, A, P[_,_]: Parser](p: => P[S, A]): P[S, List[A]] = {
      p.map { a => as: List[A] => a :: as} <*> (many(p) or Nil)
    }

    def sym[S: Symbol, P[_, _]](s: S)(implicit parser: Parser[P]): P[S, List[S]] = {
      import parser._
      symbol(s).map(s => List(s))
    }
  }

  object example {
    import algebra._
    import dsl._

    type Empty[S, A] = Boolean

    //class Empty[S, A](in: => Boolean){def eval: Boolean = in}
    //implicit def boolToEmpty[S, A](in: => Boolean): Empty[S, A] = new Empty(in)
    //implicit def emptyToBool(e: Empty[_, _]): Boolean = e.eval


    //class LazyTuple2[A, B](a: => A, b: => B) { def _1: A = a; def _2: B = b}
    //object LazyTuple2 {
      //def apply[A, B](a: => A, b: => B): LazyTuple2[A, B] = new LazyTuple2(a, b)
    //}

    type LazyTuple2[A, B] = (A, B)
    def LazyTuple2[A, B](a: A, b: B) = (a, b)

    type First[S, A] = List[S]

    type EmpFir[S, A] = LazyTuple2[Empty[S, A], First[S, A]]

    def combine[S: Symbol, A](e: => Empty[S, A])(s1: => List[S])(s2: => List[S]): List[S] = s1 union (if (e) s2 else Nil)

    object parsers {
      implicit val emptyParser: Parser[Empty] = new Parser[Empty] {
        override def empty[A, S: Symbol](a: => A): Empty[S, A] = true
        override def symbol[S: Symbol](s: => S): Empty[S, S] = false
        override def alt[A, S: Symbol](p: => Empty[S, A])(a: => Empty[S, A]): Empty[S, A] = p || a
        override def seq[A, B, S: Symbol](f: => Empty[S, A => B])(p: => Empty[S, A]): Empty[S, B] = f && p
        override def err[A, S: Symbol](p: => Empty[S, A])(a: => A, s: => String): Empty[S, A] = false
      }

      implicit val firstParser: Parser[First] = new Parser[First] {
        override def empty[A, S: Symbol](a: => A): First[S, A] = Nil
        override def symbol[S: Symbol](s: => S): First[S, S] = List(s)
        override def alt[A, S: Symbol](p: => First[S, A])(a: => First[S, A]): First[S, A] = p union a
        override def seq[A, B, S: Symbol](f: => First[S, (A) => B])(p: => First[S, A]): First[S, B] = ???
        override def err[A, S: Symbol](p: => First[S, A])(a: => A, s: => String): First[S, A] = p
      }
      
      implicit val empFir: Parser[EmpFir] = new Parser[EmpFir] {
        override def empty[A, S: Symbol](a: => A) = LazyTuple2(emptyParser.empty(a), firstParser.empty(a))
        override def symbol[S: Symbol](s: => S) = LazyTuple2(emptyParser.symbol(s), firstParser.symbol(s))
        override def alt[A, S: Symbol](p: => EmpFir[S, A])(a: => EmpFir[S, A]) = LazyTuple2(emptyParser.alt(p._1)(a._1), firstParser.alt(p._2)(a._2))
        override def seq[A, B, S: Symbol](f: => EmpFir[S, (A) => B])(p: => EmpFir[S, A]) = {
          LazyTuple2(emptyParser.seq(f._1)(p._1), combine(f._1)(f._2)(p._2))
        }

        override def err[A, S: Symbol](p: => EmpFir[S, A])(a: => A, s: => String): EmpFir[S, A] =
          LazyTuple2(emptyParser.err(p._1)(a, s), firstParser.err(p._2)(a, s))
      }
    }


    object languages {

      def agLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = {
        import parser._
        symbol('a') <|> symbol('g')
      }

      def agStarLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, List[Char]] = {
        import parser._

        sym('a').map{ c0 => c1: List[Char] => c0 ++ c1} <*> many(symbol('g')) <|> sym('a') <|> sym('b')
      }

      def emptyLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = parser.empty('f')
    }




    def run(): Unit = {
      import languages._
      import parsers._

      def checkEmpty[A, S: Symbol](p: Empty[S, A]): Boolean = p
      def invokeFirst[A, S: Symbol](p: EmpFir[S, A]): List[S] = p._2

      val emptyResult = checkEmpty(emptyLanguage[Empty])

      println(s"does the empty language accept the empty symbol? $emptyResult")

      val agResult = checkEmpty(agLanguage[Empty])
      val agFirst = invokeFirst(agLanguage[EmpFir])
      println(s"does the ag language accept the empty symbol? $agResult, $agFirst")

      val agStarResult = checkEmpty(agStarLanguage[Empty])
      val agStarFirst = invokeFirst(agStarLanguage[EmpFir])

      println(s"does the agStar language (agStarPrint) accept the empty symbol? $agStarResult, $agStarFirst")
    }
  }

  example.run()

}
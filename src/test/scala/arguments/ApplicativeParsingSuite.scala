package arguments

import scala.languageFeature.higherKinds
import org.scalatest.FunSuite

/**
  * applicative parsing according to
  * Deterministic, Error-Correcting Combinator Parsers by S. Doaitse Swierstra and Luc Duponcheel
  */
class ApplicativeParsingSuite extends FunSuite {
  trait Symbol[T]
  implicit val stringSymbol: Symbol[String] = new Symbol[String] {}
  implicit val charSymbol: Symbol[Char] = new Symbol[Char] {}

  object + {
    def unapply[A](xs: Seq[A]): Option[(A, Seq[A])] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

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

    implicit class FunctionOps[A, B](f: A => B) {
      def $[S: Symbol, P[_, _]](p: => P[S, A])(implicit parser: Parser[P]): P[S, B] = {
        parser.seq[A, B, S](parser.empty(f))(p)
      }
    }

    implicit class ParserOps[S: Symbol, A, P[_, _]: Parser](parser: P[S, A]) {
      def empty[E](a: E): P[S, E] = implicitly[Parser[P]].empty(a)
      def symbol(s: S): P[S, S] = implicitly[Parser[P]].symbol(s)
      def alt(a: => P[S, A]): P[S, A] = implicitly[Parser[P]].alt(parser)(a)

      def err(a: => A, s: => String): P[S, A] = implicitly[Parser[P]].err(parser)(a, s)
      def |(a: => P[S, A]): P[S, A] = alt(a)

      def map[B](f: A => B): P[S, B] = implicitly[Parser[P]].empty[A => B, S](f) <*> parser
      def ?(a: => A, s: => String): P[S, A] = err(a, s)
      def opt(a: => A): P[S, A] = parser | parser.empty(a)
    }

    implicit class ParserLiftedFunctionOps[S: Symbol, A, B, P[_, _]: Parser](parser: P[S, A => B]) {
      def seq(p: => P[S, A]): P[S, B] = implicitly[Parser[P]].seq(parser)(p)
      def <*>(p: => P[S, A]): P[S, B] = seq(p)
    }

    def many[S: Symbol, A, P[_,_]](p: => P[S, A])(implicit parser: Parser[P]): P[S, List[A]] = {
      ({ a: A => as: List[A] => a :: as} $ p) <*> many(p) opt Nil
    }


    /*
    def chainr1[P[_,_], S: Symbol, X](x: P[S, X], op: P[S, ]) = {
      {}
    }
    */

    def chainr[P[_,_], S: Symbol, PX](x: P[S, PX], op: P[S, PX => PX => PX])(implicit parser: Parser[P]): P[S, PX] = {
      /*
      val id = ???

      ({ x: PX => f: (PX => PX) => f(x) } $ x) <*> {

        ({ (op: PX => PX => PX) => x: PX => op(_)(x) } $ op) <*> chainr(x, op.opt(id))
      }*/

      ???
    }

    def sym[S: Symbol, P[_, _]](s: S)(implicit parser: Parser[P]): P[S, List[S]] = {
      import parser._
      symbol(s).map(s => List(s))
    }
  }

  object example {
    import algebra._
    import dsl._

    object parsers {
      object empty {

        type Empty[S, A] = Boolean

        implicit val emptyParser: Parser[Empty] = new Parser[Empty] {
          override def empty[A, S: Symbol](a: => A): Empty[S, A] = true
          override def symbol[S: Symbol](s: => S): Empty[S, S] = false
          override def alt[A, S: Symbol](p: => Empty[S, A])(a: => Empty[S, A]): Empty[S, A] = p || a
          override def seq[A, B, S: Symbol](f: => Empty[S, A => B])(p: => Empty[S, A]): Empty[S, B] = f && p
          override def err[A, S: Symbol](p: => Empty[S, A])(a: => A, s: => String): Empty[S, A] = false
        }

      }
      object first {
        import empty._

        type First[S, A] = Seq[S]

        type EmpFir[S, A] = (Empty[S, A], First[S, A])

        def combine[S: Symbol, A](e: => Empty[S, A])(s1: => Seq[S])(s2: => Seq[S]): Seq[S] = s1 union (if (e) s2 else Nil)

        implicit val firstParser: Parser[First] = new Parser[First] {
          override def empty[A, S: Symbol](a: => A): First[S, A] = Nil
          override def symbol[S: Symbol](s: => S): First[S, S] = List(s)
          override def alt[A, S: Symbol](p: => First[S, A])(a: => First[S, A]): First[S, A] = p union a
          override def seq[A, B, S: Symbol](f: => First[S, (A) => B])(p: => First[S, A]): First[S, B] = f
          override def err[A, S: Symbol](p: => First[S, A])(a: => A, s: => String): First[S, A] = p
        }

        implicit val empFir: Parser[EmpFir] = new Parser[EmpFir] {
          override def empty[A, S: Symbol](a: => A): EmpFir[S, A] = (emptyParser.empty(a), firstParser.empty(a))
          override def symbol[S: Symbol](s: => S): EmpFir[S, S] = (emptyParser.symbol(s), firstParser.symbol(s))
          override def alt[A, S: Symbol](p: => EmpFir[S, A])(a: => EmpFir[S, A]): EmpFir[S, A] = (emptyParser.alt(p._1)(a._1), firstParser.alt(p._2)(a._2))
          override def seq[A, B, S: Symbol](ef1: => EmpFir[S, (A) => B])(ef2: => EmpFir[S, A]): EmpFir[S, B] = {
            (emptyParser.seq(ef1._1)(ef2._1), combine(ef1._1)(ef1._2)(ef2._2))
          }

          override def err[A, S: Symbol](p: => EmpFir[S, A])(a: => A, s: => String): EmpFir[S, A] =
            (emptyParser.err(p._1)(a, s), firstParser.err(p._2)(a, s))
        }
      }

      object deterministic {

        import first._

        type Input[S] = Seq[S]
        type Follow[S] = Seq[S]
        type DetParFun[S, A] = Input[S] => Follow[S] => (A, Input[S])

        type DetPar[S, A] = (EmpFir[S, A], DetParFun[S, A])

        implicit val detPar: Parser[DetPar] = new Parser[DetPar] {

          def pempty[S, A](a: A): Seq[S] => Seq[S] => (A, Seq[S]) = { input => _ => (a, input)}
          def psymbol[S](s: S): Seq[S] => Seq[S] => (S, Seq[S]) = {
            case _ +: tail => _ => (s, tail)
          }

          override def empty[A, S: Symbol](a: => A): DetPar[S, A] = (empFir.empty(a), pempty(a))
          override def symbol[S: Symbol](s: => S): DetPar[S, S] = (empFir.symbol(s), psymbol(s))
          override def err[A, S: Symbol](p: => DetPar[S, A])(a: => A, s: => String) = {
            alt(p)((empFir.empty(a), pempty(a)))
          }


          override def alt[A, S: Symbol](p: => DetPar[S, A])(a: => DetPar[S, A]): DetPar[S, A] = (p, a) match {
            case ((ef1 @ (e1, f1), p1),  (ef2 @ (e2, f2), p2) ) =>
              def palt(p1: DetParFun[S, A])(p2: DetParFun[S, A]): DetParFun[S, A] = {
                case Seq() => follow =>
                    if (e1) p1(Nil)(follow)
                    else if (e2) p2(Nil)(follow)
                    else sys.error("Unexpected EOF")
                case inp @ (s +: _) => follow =>
                    if (f1.contains(s)) p1(inp)(follow)
                    else if (f2.contains(s)) p2(inp)(follow)
                    else if (e1 && follow.contains(s)) p1(inp)(follow)
                    else if (e2 && follow.contains(s)) p2(inp)(follow)
                    else sys.error("Illegal input symbol: " + s)
              }
              (empFir.alt(ef1)(ef2), palt(p1)(p2))
          }

          override def seq[A, B, S: Symbol](f: => DetPar[S, (A) => B])(p: => DetPar[S, A]): DetPar[S, B] = (f, p) match {
            case ((ef1, p1), (ef2 @ (e2, f2), p2)) =>
              def pseq(p1: DetParFun[S, A => B])(p2: DetParFun[S, A]): DetParFun[S, B] = {
                inp => follow =>

                  val (v1, inp1) = p1(inp)(combine(e2)(f2)(follow)) //s1 union (if (e) s2 else Nil)
                  val (v2, inp2) = p2(inp1)(follow)
                  (v1(v2), inp2)
              }
              (ef1.seq(ef2), pseq(p1)(p2))
          }
        }
      }

      object errorcorrection {
        sealed trait EmptyDesc[S, A]
        case class IsEmpty[S, A](a: A) extends EmptyDesc[S, A]
        case class Insert[S, A](a: A, message: String) extends EmptyDesc[S, A]

        type State[S] = (List[S], String)
        type Noskip[S] = List[List[S]]
        type ErrParFun[S, A] = State[S] => Noskip[S] => (A, State[S])

        case class Look[S, A](s: S, a: A)
        type ParserTab[S, A] = List[Look[S, ErrParFun[S, A]]]

        def edempty[S, A](a: A): IsEmpty[S, A] = IsEmpty(a)
        def edsymbol[S, A](a: A): Insert[S, A] = Insert(a, s"Inserted $a")
        def ederr[S, A](a: A, message: String): Insert[S, A] = Insert(a, message)
        def edalt[S, A](edp: EmptyDesc[S, A], edq: EmptyDesc[S, A]): EmptyDesc[S, A] = edp match {
          case _: IsEmpty[S, A] => edp
          case _: Insert[S, A] => edq
        }

        def edseq[S, A, B](edp: EmptyDesc[S, A => B], edq: EmptyDesc[S, A]) = (edp, edq) match {
          case (IsEmpty(pv), IsEmpty(qv)) => IsEmpty(pv(qv))
          case (IsEmpty(pv), Insert(qv, sq)) => Insert(pv(qv), sq)
          case (Insert(pv, sp), IsEmpty(qv)) => Insert(pv(qv), sp)
          case (Insert(pv, sp), Insert(qv, sq)) => Insert(pv(qv), sp ++ sq)
        }


        def tempty(a: Any) = Nil
      }

    }

    object languages {

      def agLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = {
        import parser._
        symbol('a') | symbol('g')
      }

      def agStarLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, List[Char]] = {
        import parser._

        ({ c0: List[Char] => c1: List[Char] => c0 ++ c1 } $ sym('a')) <*> many(symbol('g'))
      }

      def emptyLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = parser.empty('e')
      def abOrEmptyLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Seq[Char]] = {
        //val ab = parser.seq(parser.symbol('a'), parser.symbol('b))

        import parser._
        import dsl._
        val ab: P[Char, Seq[Char]] = ({ a: Char => b: Char =>  Seq(a, b) } $ symbol('a')) <*> symbol('b')

        val e: P[Char, Seq[Char]] = empty[Seq[Char], Char](List('e'))

        parser.alt(ab)(e)
      }

      def simpleLanguage[P[_, _]](implicit parser: Parser[P]) = {
        import parser._


        val f = { s: List[Char] => x: List[Char] => y: List[Char] => x ++ s ++ y }
        val g: P[Char, (List[Char]) => (List[Char]) => List[Char]] = f $ sym(';')


        //val f_seq_sym: P[Char, (List[Char]) => (List[Char]) => List[Char]] = f $ sym(';')

        // def chainr(x, sym: List[Char] => List[Char]: P[Char

        def stats = chainr(stat, f $ sym(';'))
        def stat = ???

        def assignment: P[Char, List[Char]] = sym('a')
        def cond: P[Char, List[Char]] = sym('c')
        val then_part: P[Char, List[Char]] = ???

        val if_stat =
          ({ i: List[Char] => c: List[Char] => tp: List[Char] => ep: List[Char] => f: List[Char] => i ++ c ++ tp ++ ep ++ f} $ sym('I')) <*> cond <*> then_part



      }
    }

  }

  import example._

  test("check empty") {
    import languages._
    import parsers.empty._

    def checkEmpty[A, S: Symbol](p: Empty[S, A]): Boolean = p

    val emptyResult = checkEmpty(emptyLanguage[Empty])
    assert(emptyResult === true, "Empty language should accept the empty symbol")

    val abOrEmptyResult = checkEmpty(abOrEmptyLanguage[Empty])
    assert(abOrEmptyResult === true, "AB|'' should accept the empty symbol")

    val agResult = checkEmpty(agLanguage[Empty])
    assert(agResult === false, "AG language should not accept the empty symbol")

    val agStarResult = checkEmpty(agStarLanguage[Empty])
    assert(agStarResult === false, "AG* language should not accept the empty symbol")


  }

  test("check languages") {

    import languages._
    import parsers.empty._
    import parsers.first._
    import parsers.deterministic._

    def checkEmpty[A, S: Symbol](p: Empty[S, A]): Boolean = p
    def invokeFirst[A, S: Symbol](p: EmpFir[S, A]): Seq[S] = p._2
    def invokeDetParser[A, S: Symbol]( p: DetPar[S, A] )( input: Input[S]): A = p match {
      case (_, p) =>
        val (a, _) = p(input)(Nil)
        a
    }


    val agDetPar = invokeDetParser(agLanguage[DetPar])("ag".toList)
    assert(agDetPar === 'a')


    val abOrEmptyFirst = invokeFirst(abOrEmptyLanguage[EmpFir])

    assert(abOrEmptyFirst ===  "a".toSeq)

    val abOrEmptyDetParResult = invokeDetParser(abOrEmptyLanguage[DetPar])(Seq())
    assert(abOrEmptyDetParResult === "ab".toSeq)

    val agStarFirst = invokeFirst(agStarLanguage[EmpFir])
    assert(agStarFirst === "a".toSeq)

  }

  def reader[T](it: T): Seq[T] => (T, Seq[T]) = {
    case head +: tail => (it, tail)
  }

}
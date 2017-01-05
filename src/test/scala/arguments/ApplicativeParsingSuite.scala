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

  trait Parser[P[_, _]] {

    def empty[A, S: Symbol](a: A): P[S, A]
    def symbol[S: Symbol](s: S): P[S, S]

    def alt[A, S: Symbol](p: P[S, A])(a: P[S, A]): P[S, A]
    def seq[A, B, S: Symbol](p: P[S, A])(f: P[S, A => B]): P[S, B]
    def err[A, S: Symbol](p: P[S, A])(a: A, s: String): P[S, A]
  }

  implicit class ParserOps[S: Symbol, A, P[_, _]: Parser](parser: P[S, A]) {
    def empty[E](a: E): P[S, E] = implicitly[Parser[P]].empty(a)
    def symbol(s: S): P[S, S] = implicitly[Parser[P]].symbol(s)
    def alt(a: P[S, A]): P[S, A] = implicitly[Parser[P]].alt(parser)(a)
    def seq[B](ap: P[S, A => B]): P[S, B] = implicitly[Parser[P]].seq(parser)(ap)
    def err(a: A, s: String): P[S, A] = implicitly[Parser[P]].err(parser)(a, s)
    def <|>(a: P[S, A]): P[S, A] = alt(a)
    def <*>[B](ap: P[S, A => B]): P[S, B] = seq(ap)
    def |?|(a: A, s: String): P[S, A] = err(a, s)
    def map[B](f: A => B): P[S, B] = parser <*> parser.empty(f)
    def opt(a: A): P[S, A] = parser <|> parser.empty(a)
  }

  def many[S: Symbol, A, P[_,_]: Parser](p: P[S, A]): P[S, List[A]] = {
    val manypOrNil: P[S, List[A]] = many(p).opt(Nil)

    val buildList: P[S, (A) => (List[A]) => List[A]] = p.empty{ a: A => as: List[A] => a :: as }

    manypOrNil.seq(p.seq(buildList))
  }

  case class Empty[S, A](value: Boolean)
  case class Print[S, A](value: String)

  implicit val emptyParser: Parser[Empty] = new Parser[Empty] {
    override def empty[A, S: Symbol](a: A): Empty[S, A] = Empty(true)
    override def symbol[S: Symbol](s: S): Empty[S, S] = Empty(false)
    override def alt[A, S: Symbol](p: Empty[S, A])(a: Empty[S, A]): Empty[S, A] = Empty( p.value || a.value)
    override def seq[A, B, S: Symbol](p: Empty[S, A])(f: Empty[S, (A) => B]): Empty[S, B] = Empty(p.value && f.value)
    override def err[A, S: Symbol](p: Empty[S, A])(a: A, s: String): Empty[S, A] = Empty(false)
  }

  implicit val printerParser: Parser[Print] = new Parser[Print] {
    override def empty[A, S: Symbol](a: A): Print[S, A] = Print("[]")
    override def symbol[S: Symbol](s: S): Print[S, S] = Print(s"$s")
    override def alt[A, S: Symbol](p: Print[S, A])(a: Print[S, A]): Print[S, A] = Print( p.value +  "||" + a.value)
    override def seq[A, B, S: Symbol](p: Print[S, A])(f: Print[S, (A) => B]): Print[S, B] = Print(p.value + "&&" + f.value)
    override def err[A, S: Symbol](p: Print[S, A])(a: A, s: String): Print[S, A] = Print("E: s")
  }


  def agLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = {
    import parser._
    alt(symbol('a'))(symbol('g'))
  }
  def emptyLanguage[P[_, _]](implicit parser: Parser[P]): P[Char, Char] = parser.empty('f')

  def checkEmpty[A, S: Symbol](p: Empty[S, A]): Boolean = p.value
  def print[A, S: Symbol](p: Print[S, A]): String = p.value

  val emptyResult = checkEmpty(emptyLanguage[Empty])
  val emptyPrint = print(emptyLanguage[Print])
  println(s"does the empty language ($emptyPrint) accept the empty symbol? $emptyResult")

  val agResult = checkEmpty(agLanguage[Empty])
  val agPrint =  print(agLanguage[Print])
  println(s"does the ag language ($agPrint) accept the empty symbol? $agResult")




}
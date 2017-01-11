package arguments.naive

import arguments.TrySyntax._
import arguments.EitherSyntax._
import scala.util.Try

case object NoArgument extends Argument[Unit] {
  override def consume(args: Seq[String]): Result[(Seq[String], Unit)] = Right((args, ()))

  override def usage: String = ""
}

case class Command[C, P](cName: String, child: Argument[C])(implicit reads: Reads[C, P]) extends Argument[P] {

  val helpFlag = new HelpFlag()

  sealed trait State
  case object NotFound extends State
  case object Found extends State
  case object HelpRequestFound extends State

  override def consume(args: Seq[String]): Result[(Seq[String], P)] = {
    val (before, after, state) =
      args.foldLeft((Seq[String](), Seq[String](), NotFound: State)) { case ((before, after, state), curr) =>
        state match {
          case NotFound if curr != cName  => (before :+ curr, after, state)
          case NotFound if curr == cName => (before, after, Found)
          case Found if curr == "--help" => (before, after :+ curr, HelpRequestFound)
          case Found if curr == "-h" => (before, after :+ curr, HelpRequestFound)
          case Found => (before, after :+ curr, Found)
          case HelpRequestFound => (before, after, HelpRequestFound)
        }
      }

    state match {
      case NotFound => Left(ArgumentExpected(this))
      case Found => child.consume(after).right.flatMap { case (remaining, value) =>

        reads.read(value).toEither(CouldNotReadValue(value, _)).map { readValue =>
          (before ++ remaining, readValue)
        }
      }
      case HelpRequestFound => Left(HelpRequested(this))
    }
  }

  override def usage: String = s"$cName ${child.usage}"
}

object Command {
  def build[C, P](identifier: String, child: Argument[C])(builder: C => P): Command[C, P] = {
    new Command[C, P](identifier, child)(new Reads[C, P]{
      override def read(s: C): Try[P] = Try(builder(s))
    })
  }
}

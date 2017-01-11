package arguments.naive

class HelpFlag extends Flag[Boolean]('h', (true, false)) {

  override def consume(args: Seq[String]): Result[(Seq[String], Boolean)] = {
    super.consume(args).right.flatMap {  case (_ , found) =>
      if(found) Left(HelpRequested(this))
      else Right((args, found))
    }
  }
}

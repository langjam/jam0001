package vrv

sealed trait Content
object Content {
  def commentOf(cont: Content*): Comment = Comment(cont)
  def firstClassOf(syms: String*): Comment = Comment(Seq(Symbols(syms)))
  def symbolsOf(syms: String*): Symbols = Symbols(syms)

  def isEmpty(content: Content): Boolean =
    content match {
      case Symbols(syms) => syms.isEmpty
      case Comment(cont) => cont.isEmpty
    }

  def isComment(content: Content): Boolean =
    content match {
      case _: Comment => true
      case _          => false
    }

  implicit val pp: Show[Content] = new Show[Content] {
    override def show(content: Content): String =
      content match {
        case x: Symbols => Show(x)
        case x: Comment => Show(x)
      }
  }
}

final case class Comment(content: Seq[Content]) extends Content
object Comment {
  // A comment is said to be first-class if it can be evaluated,
  // given that it has no inner comments that must be evaluated first.
  //
  // First class comments can always be represented with single line syntax: //
  // otherwise it should be rendered with /* */ to allows nesting other comments in it.
  object FirstClass {
    def isFirstClass(comment: Comment): Boolean =
      !comment.content.exists {
        case x: Comment => true
        case _          => false
      }

    // Obtain a flattened list of first-level symbols on this comment.
    def firstClassSyms(comment: Comment): Seq[String] =
      comment.content.collect {
        case x: Symbols => x.symbols
      }.flatten

  }

  implicit val pp: Show[Comment] = new Show[Comment] {
    override def show(comment: Comment): String =
      if (FirstClass.isFirstClass(comment))
        comment.content.map(Show(_)).mkString("// ", " ", "\n")
      else comment.content.map(Show(_)).mkString("/* ", "  ", " */")
  }
}

final case class Symbols(symbols: Seq[String]) extends Content
object Symbols {
  def empty: Symbols = Symbols(Nil)

  implicit val pp: Show[Symbols] = new Show[Symbols] {
    override def show(content: Symbols): String =
      content.symbols.mkString(" ")
  }
}

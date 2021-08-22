package vrv

import fastparse._
import NoWhitespace._

object Parser {
  def ws[_: P] = " " | "\t"
  def nl[_: P] = "\n" | "\r"

  def slashSlash[_: P] = "//" ~ ws.rep
  def slashStar[_: P] = "/*" ~ (nl | ws ~ "*".rep).rep
  def starSlash[_: P] = (nl | ws ~ (!"*/" ~ "*").rep).rep ~ "*/"

  def symbolSeparator[_: P] = ws
  def symbolTerminator[_: P] = nl | ws | commentStart | starSlash

  def symbol[_: P] = (!symbolTerminator ~ AnyChar).rep(1)

  def symbols[_: P]: P[Symbols] =
    symbol.!.rep(sep = symbolSeparator.rep(1))
      .map(_.filterNot(_.isEmpty))
      .map(Symbols(_))

  def commentStart[_: P] = slashSlash | slashStar

  def onelinerComment[_: P] =
    slashSlash ~/ symbols.map(ss => Comment(Seq(ss)))

  def multilineComment[_: P]: P[Comment] =
    slashStar ~/ contents.map(Comment(_)) ~ starSlash

  def comment[_: P]: P[Comment] = multilineComment | onelinerComment

  def contentSeparator[_: P] = ws | nl

  def contents[_: P]: P[Seq[Content]] =
    (comment | symbols)
      .rep(sep = contentSeparator.rep(1))
      .map(_.filterNot(Content.isEmpty))

  def apply(code: String) = fastparse.parse(code, contents(_))
}

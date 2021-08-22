package tracecom

import tracecom.Syntax.Definition.Function
import tracecom.Syntax.Expr.*
import tracecom.Syntax.{Comment, Expr, Statement}
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.{Numbers, Parser, Parser0}
import zio.{IO, ZIO}


object Parse:

  def parseSource(source: String): IO[String, List[Function]] =
    ZIO.fromEither(Parse.functions.parseAll(source)).mapError(error => s"parser error: $error")

  val whitespace: Parser[Unit] = Parser.charIn(" \t\r\n").void
  val whitespaces: Parser0[Unit] = whitespace.rep.void
  val whitespaces0: Parser0[Unit] = whitespace.rep0.void

  val listSep: Parser[Unit] = Parser.char(',').surroundedBy(whitespaces0).void

  val name: Parser[String] = Parser.charIn(('a' to 'z').toList ++ ('A' to 'Z').toList).rep.map(_.toList.mkString)

  val int: Parser[Int] = Numbers.bigInt.map(_.toInt)

  val comment: Parser[Comment] = Parser.product01(Parser.char('#').?, Parser.charsWhile(_ != '>').rep.map(_.toList.mkString)).between(Parser.char('<'), Parser.char('>'))
    .map((trace, comment1) => Comment(comment1, trace.as(true).getOrElse(false)))

  val expression: Parser[Expr] = Parser.recursive[Expr] { (expression1: Parser[Expr]) =>

    val literal = (int ~ comment.?)
      .map(Literal.apply)

    val parens: Parser[Expr] = (expression1.between(Parser.char('('), Parser.char(')')) ~ comment.?)
      .map(Parens.apply)

    val statementExpr: Parser[Statement.Expr] = expression1
      .map(Statement.Expr.apply)

    val statementLet: Parser[Statement.Let] = ((Parser.string("let") *> name.surroundedBy(whitespaces) <* Parser.char('=')) ~ expression1)
      .map(Statement.Let.apply)

    val statement: Parser[Statement] = Parser.oneOf(statementLet :: statementExpr :: Nil)

    val block: Parser[Block] = statement.surroundedBy(whitespaces0).rep.between(Parser.char('{'), Parser.char('}'))
      .map(Block(_, None))

    val parseIf: Parser[If] = ((Parser.string("if") *> whitespaces *> expression1.between(Parser.char('('), Parser.char(')')) <* whitespaces) ~ (expression1 <* Parser.string("else")) ~ expression1).flattenTuple
      .map(If.apply(_, _, _, None))

    val call: Parser[Call] = (name ~ expression1.repSep0(listSep).between(Parser.char('('), Parser.char(')')) ~ comment.?).backtrack.flattenTuple
      .map(Call.apply)

    val ref: Parser[Ref] = (name ~ comment.?)
      .map(Ref.apply)

    val factor: Parser[Expr] = Parser.oneOf(literal :: parens :: block :: parseIf :: call :: ref :: Nil).surroundedBy(whitespaces0)

    def opLevel2Expr(exprHead: Expr, exprs: List[(String, Expr)]): Expr = exprs.foldLeft(exprHead)({case (expr1, (op, expr2)) => Call("op" ++ op, List(expr1, expr2), None)})

    val opLevel2: Parser[Expr] = (factor ~ (Parser.stringIn(List("*")) ~ factor).rep0)
      .map(opLevel2Expr)

    def opLevel1Expr(exprHead: Expr, exprs: List[(String, Expr)]): Expr = exprs.foldLeft(exprHead)({case (expr1, (op, expr2)) => Call("op" ++ op, List(expr1, expr2), None)})

    val opLevel1: Parser[Expr] = (opLevel2 ~ (Parser.stringIn(List("+", "-")) ~ opLevel2).rep0)
      .map(opLevel1Expr)

    opLevel1
  }

  val function: Parser[Function] = ((Parser.string("def") *> whitespaces *> name) ~ (name.repSep0(listSep).between(Parser.char('('), Parser.char(')'))) ~ (whitespaces *> Parser.char('=') *> whitespaces *> expression)).flattenTuple
    .map(Function.apply)

  val functions: Parser0[List[Function]] = function.rep0

  extension [A, B, C](self: Parser[((A, B), C)])
    def flattenTuple: Parser[(A, B, C)] = self.map({case ((a, b), c) => (a, b, c)})

package tracecom

import tracecom.Syntax.Expr
import tracecom.Syntax.Expr.{Call, Literal, Parens, Ref}
import cats.data.NonEmptyList


object Syntax:

  enum Definition:
    case Function(name: String, parameters: List[String], expr: Expr)

    def structurePrint: String = this match
      case Function(name, parameters, expr) =>
        s"""Function(
           |  name = $name
           |  expr = ${expr.structurePrint(1)}
           |)""".stripMargin

  enum Statement:
    case Expr(expr: Syntax.Expr)
    case Let(name: String, expr: Syntax.Expr)

  enum Expr:
    case Literal(value: Int, comment: Option[Comment])
    case Parens(expr: Expr, comment: Option[Comment])
    case Ref(name: String, comment: Option[Comment])
    case Call(name: String, arguments: List[Expr], comment: Option[Comment])
    case If(condition: Expr, exprTrue: Expr, exprFalse: Expr, comment: Option[Comment])
    case Block(statments: NonEmptyList[Statement], comment: Option[Comment])

    val comment: Option[Comment]

    def structurePrint(indentationLevel: Int): String = this match
      case Literal(value, comment) =>
        s"Literal(value = $value, comment = ${comment.getOrElse("")})"

      case Parens(expr, comment) =>
        "Parens(\n" ++
        s"""  expr = ${expr.structurePrint(indentationLevel + 1)}
           |  comment = ${comment.getOrElse("")}""".stripMargin.indent(indentationLevel)

      case Ref(name, comment) =>
        s"Ref(name = $name, comment = ${comment.getOrElse("")})"

      case Call(name, arguments, comment) =>
        val prettyArguments = arguments.zipWithIndex.map((argument, index) => s"  argument$index = ${argument.structurePrint(indentationLevel + 1)}").mkString("\n")
        "Call(\n" ++
        s"""  name = $name
           |$prettyArguments
           |  comment = ${comment.getOrElse("")}
           |)""".stripMargin.indent(indentationLevel)

      case If(condition, exprTrue, exprFalse, comment) =>
        "If(\n" ++
          s"""  condition = ${condition.structurePrint(indentationLevel + 1)}
             |  exprTrue = ${exprTrue.structurePrint(indentationLevel + 1)}
             |  exprFalse = ${exprFalse.structurePrint(indentationLevel + 1)}
             |  comment = ${comment.getOrElse("")}
             |)""".stripMargin.indent(indentationLevel)

      case Block(statements, comment) => s"{}" // TODO

    def prettyPrint: String = this match
      case Literal(value, _) => value.toString
      case Parens(expr1, _) => s"(${expr1.prettyPrint})"
      case Ref(name, _) => name
      case Call(name, arguments, _) => s"$name(${arguments.map(_.prettyPrint).mkString(", ")})"
      case If(condition, exprTrue, exprFalse, _) => s"if (${condition.prettyPrint}) ${exprTrue.prettyPrint} else ${exprFalse.prettyPrint}"
      case Block(statments, _) => "" // TODO

  case class Comment(message: String, trace: Boolean)

package tracecom

import tracecom.Syntax.Definition.*
import tracecom.Syntax.Expr.*
import tracecom.Syntax.{Comment, Expr, Statement}
import cats.implicits.*
import zio.interop.catz.*
import zio.{IO, ZIO}


object Simplify:

  def simplifyFunction(function: Function): IO[String, Function] = for {
    expr <- simplifyExpr(function.expr)
  } yield function.copy(expr = expr)

  def simplifyExpr(expr: Expr): IO[String, Expr] = expr match
    case Parens(expr1, Some(comment)) => for {
      expr1a <- simplifyExpr(expr1)
      result <- addComment(expr1a, comment)
    } yield result

    case Parens(expr1, _) => for {
      expr1a <- simplifyExpr(expr1)
    } yield expr1a

    case Call(name, arguments, comment) => for {
      arguments1 <- arguments.traverse(simplifyExpr)
    } yield Call(name, arguments1, comment)

    case If(condition, exprTrue, exprFalse, comment) => for {
      condition1 <- simplifyExpr(condition)
      exprTrue1 <- simplifyExpr(exprTrue)
      exprFalse1 <- simplifyExpr(exprFalse)
    } yield If(condition1, exprTrue1, exprFalse1, comment)

    case Block(statements, comment) => for {
      statements1 <- statements.traverse(simplifyStatement)
    } yield Block(statements1, comment)

    case _ =>
      ZIO.succeed(expr)

    def simplifyStatement(statement: Statement): IO[String, Statement] = statement match
      case Statement.Expr(expr) => for {
        expr1 <- simplifyExpr(expr)
      } yield Statement.Expr(expr1)

      case Statement.Let(name, expr) => for {
        expr1 <- simplifyExpr(expr)
      } yield Statement.Let(name, expr1)

  def addComment(expr: Expr, comment: Comment): IO[String, Expr] = expr match
    case Literal(value, None) => ZIO.succeed(Literal(value, Some(comment)))
    case Parens(expr1, None) => ZIO.succeed(Parens(expr, Some(comment)))
    case Ref(name, None) => ZIO.succeed(Ref(name, Some(comment)))
    case Call(name, arguments, None) => ZIO.succeed(Call(name, arguments, Some(comment)))
    case If(condition, exprTrue, exprFalse, None) => ZIO.succeed(If(condition, exprTrue, exprFalse, Some(comment)))
    case _ => ZIO.fail(s"expression $expr already has a comment")

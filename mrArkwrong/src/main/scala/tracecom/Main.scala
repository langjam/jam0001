package tracecom

import tracecom.Evaluate.{State, evaluate}
import tracecom.Parse.parseSource
import tracecom.Simplify.simplifyFunction
import tracecom.Syntax.Definition.Function
import tracecom.Syntax.Expr.*
import cats.Monad
import cats.implicits.*
import cats.parse.{Numbers, Parser, Parser0}
import zio.blocking.{Blocking, blocking}
import zio.console.*
import zio.interop.catz.*
import zio.interop.catz.implicits.*
import zio.*

import java.io.IOException
import scala.io.Source


object Main extends zio.App:

  def run(args: List[String]): URIO[ZEnv, ExitCode] = run1.catchAll { error =>
    putStrLn(s"error: $error")
  }.exitCode

  private val run1: ZIO[ZEnv, String, ExitCode] = for {
    _ <- ZIO.succeed(())

    source: String <- readFile("example.txt").mapError(error => s"reading file failed: $error")

    functions: List[Function] <- parseSource(source)
    //_ <- functions.traverse(function => putStrLn(function.structurePrint ++ "\n").ignore)

    functions1: List[Function] <- functions.traverse(simplifyFunction)
    //_ <- functions1.traverse(function => putStrLn(function.structurePrint ++ "\n").ignore)

    mainFunction: Function <- functions1.find(_.name == "main").toZio("main function not found")

    result: Int <- evaluate(mainFunction.expr, State(functions1))

    _ <- putStrLn(s"result: $result").ignore
  } yield ExitCode.success

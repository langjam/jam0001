package tracecom

import tracecom.Evaluate.Builtin
import tracecom.Syntax.Definition.Function
import tracecom.Syntax.Expr.*
import tracecom.Syntax.{Comment, Expr, Statement}
import cats.implicits.*
import zio.interop.catz.*
import zio.console.*
import zio.{IO, URIO, ZEnv, ZIO}


object Evaluate:

  case class EvaluatedValue(name: String, value: Int)

  case class Builtin(name: String, call: List[Int] => ZIO[ZEnv, String, Int])

  val builtins: List[Builtin] = List(
    Builtin("print", getArguments1(_, "print") >>= (arg => putStrLn(arg.toString).ignore.as(0))),
    Builtin("op+", getArguments2(_, "op+") >>= ((arg1, arg2) => ZIO.succeed(arg1 + arg2))),
    Builtin("op-", getArguments2(_, "op+") >>= ((arg1, arg2) => ZIO.succeed(arg1 - arg2))),
    Builtin("op*", getArguments2(_, "op+") >>= ((arg1, arg2) => ZIO.succeed(arg1 * arg2)))
  )

  def getArguments0(arguments: List[Int], name: String): IO[String, Unit] = arguments match
    case List() => ZIO.succeed(())
    case _ => wrongArgumentNumber(name)

  def getArguments1(arguments: List[Int], name: String): IO[String, Int] = arguments match
    case List(arg) => ZIO.succeed(arg)
    case _ => wrongArgumentNumber(name)

  def getArguments2(arguments: List[Int], name: String): IO[String, (Int, Int)] = arguments match
    case List(arg1, arg2) => ZIO.succeed((arg1, arg2))
    case _ => wrongArgumentNumber(name)

  def wrongArgumentNumber[A](name: String): IO[String, A] = ZIO.fail(s"wrong number of arguments for function $name()")

  case class State(functions: List[Function], values: List[EvaluatedValue] = List()):
    def addValues(values1: List[EvaluatedValue]): State = this.copy(values = values1 ++ values)

  def evaluate(expr: Expr, state: State): ZIO[ZEnv, String, Int] = for {
    result <- evaluate1(expr, state)
    _ = expr.comment.map(comment1 => evaluateComment(expr, result, comment1))
  } yield result

  def evaluate1(expr: Expr, state: State): ZIO[ZEnv, String, Int] = expr match
    case Literal(value, _) =>
      ZIO.succeed(value)

    case Parens(expr1, _) =>
      evaluate(expr1, state)

    case Ref(name, _) => for {
      value <- state.values.find(_.name == name).toZio(s"value $name not defined")
    } yield value.value

    case Call(name, arguments, _) => for {
      callable <- builtins.find(_.name == name).map(InstanceValue[Callable, Builtin].apply).orElse(
        state.functions.find(_.name == name).map(InstanceValue[Callable, Function].apply)
      ).toZio(s"function $name() not found")

      argumentResults <- arguments.traverse(evaluate(_, state))

      result <- callable.call(argumentResults, state)
    } yield result

    case If(condition, exprTrue, exprFalse, _) => for {
      conditionResult <- evaluate(condition, state)
      result <- if (conditionResult != 0) evaluate(exprTrue, state) else evaluate(exprFalse, state)
    } yield result

    case Block(statements, _) => for {
      foo <- evaluateStatement(statements.head, state)
      result <- statements.tail.foldLeftM(foo)({case ((result, state1), statement) => evaluateStatement(statement, state1)})
    } yield result._1

  def evaluateStatement(statement: Statement, state: State): ZIO[ZEnv, String, (Int, State)] = statement match
    case Statement.Expr(expr) => for {
      result <- evaluate(expr, state)
    } yield (result, state)

    case Statement.Let(name, expr) => for {
      result <- evaluate(expr, state)
    } yield (0, state.copy(values = EvaluatedValue(name, result) +: state.values))

  def evaluateComment(expr: Expr, result: Int, comment: Comment): Unit = {
    if (comment.trace) println(s"trace ${expr.prettyPrint} = $result <${comment.message}>")
  }

  trait Callable[T]:
    extension (self: T) def call(argumentResults: List[Int], state: State): ZIO[ZEnv, String, Int]

  given Callable[Function] with
    extension (self: Function) def call(argumentResults: List[Int], state: State): ZIO[ZEnv, String, Int] = for {
      argumentValues <- safeZip(self.parameters, argumentResults).toZio(s"wrong number of arguments for ${self.name}")
      argumentValues1 = argumentValues.map({case (parameter, argumentResult) => EvaluatedValue(parameter, argumentResult)})
      result <- evaluate(self.expr, state.addValues(argumentValues1))
    } yield result

  given Callable[Builtin] with
    extension (self: Builtin) def call(argumentResults: List[Int], state: State): ZIO[ZEnv, String, Int] =
      self.call(argumentResults)

  given [A]: Callable[InstanceValue[Callable, A]] with
    extension (self: InstanceValue[Callable, A]) def call(argumentResults: List[Int], state: State): ZIO[ZEnv, String, Int] =
      given Callable[A] = self.instance
      self.value.call(argumentResults, state)

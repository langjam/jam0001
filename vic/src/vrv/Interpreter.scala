package vrv

import vrv.Builtins.firstClass
import vrv.Runtime._

import scala.util.chaining._

object Interpreter {

  val builtins = Seq(
    Builtins.load,
    Builtins.loadURL,
//    Builtins.bigDecimal,
    Builtins.missing,
//    Builtins.panic,
//    Builtins.show,
    Builtins.hello
  )

  // Left(arg) if it was not possible to reduce, otherwise Right(res)
  type MaybeReduced = Either[Arg, Res]
  type Evaluator = Arg => MaybeReduced

  def evaluator(runtime: Runtime): Evaluator = {
    val reduction = runtime.reductions.reduceLeft(_ orElse _)
    reduction.andThen(Right(_)).orElse { case arg => Left(arg) }
  }

  def evalComment(
      runtime: Runtime,
      comment: Comment,
      left: Seq[Content] = Nil,
      right: Seq[Content] = Nil
  ): MaybeReduced = Arg(runtime, left, comment, right).pipe(evaluator(runtime))

  def reduceContent(
      runtime: Runtime,
      left: Seq[Content],
      center: Option[Content],
      right: Seq[Content]
  ): (Runtime, Seq[Content]) =
    (left, center, right) match {
      case (l, None, r) =>
        (runtime, l ++ r)

      case (l, Some(s: Symbols), Nil) =>
        (runtime, l :+ s)

      case (l, Some(s: Symbols), r) =>
        reduceContent(runtime, l :+ s, r.headOption, r.tail)

      case (l, firstClass(c), r) =>
        evalComment(runtime, c, l, r) match {
          case Right(Res(newRuntime, result)) =>
            reduceContent(newRuntime, result)
          case Left(_) if r.nonEmpty =>
            reduceContent(runtime, l :+ c, r.headOption, r.tail)
          case Left(_) =>
            (runtime, l :+ c)
        }

      case (l, Some(Comment(content)), r) =>
        val (newRuntime, newContent) = reduceContent(runtime, content)
        val notReduced = newContent == content
        if (notReduced && r.isEmpty)
          (runtime, l :+ Comment(content))
        else if (notReduced)
          reduceContent(runtime, l :+ Comment(content), r.headOption, r.tail)
        else
          reduceContent(newRuntime, l, Some(Comment(newContent)), r)
    }

  def reduceContent(
      runtime: Runtime,
      content: Seq[Content]
  ): (Runtime, Seq[Content]) = {
    reduceContent(
      runtime,
      left = Nil,
      center = content.headOption,
      right = content.tail
    )
  }

  def reduce(runtime: Runtime, comment: Comment): Comment = {
    val (newRuntime, newContent) = reduceContent(runtime, comment.content)
    if (newContent.size == 1 && newContent.exists(Content.isComment))
      newContent.collect { case c: Comment => c }.head
    else Comment(newContent)
  }

  def apply(contents: Seq[Content]): Comment = {
    val runtime = Runtime(builtins)
    val topComment = Comment(contents)
    pprint.pprintln("Running" -> topComment)
    reduce(runtime, topComment)
  }
}

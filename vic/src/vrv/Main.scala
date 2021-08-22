package vrv

import os._
import scala.util.Try

object Main {

  def help(): Unit = println(os.read(os.pwd / "README.md"))

  def parse(path: Path): Unit = {
    println(s"Parsing ${path}")
    val ast = Parser(os.read(path))
    pprint.pprintln(ast)
  }

  def run(path: Path): Unit = {
    pprint.pprintln(s"Running ${path}")
    val fastparse.Parsed.Success(ast, _) = Parser(os.read(path))
    val firstClassComment = Interpreter(ast)
    val code = Show(firstClassComment)
    println(code)
  }

  def toPath(s: String): Path = os.Path(s, os.pwd)

  def main(args: Array[String]): Unit =
    args.toList match {
      case "parse" :: path :: _ =>
        parse(toPath(path))

      case "run" :: path :: _ =>
        run(toPath(path))

      case _ =>
        help()
    }

}

package vrv

import Runtime.{Arg, Reduction, Res}
import Comment.FirstClass
import FirstClass.{firstClassSyms, isFirstClass}
import Content.{commentOf, firstClassOf, symbolsOf}

import java.net.URI
import scala.io.Source
import scala.util.Try

object Builtins {

  object firstClass {
    def unapply(fc: Option[Content]): Option[Comment] =
      fc match {
        case Some(c: Comment) if isFirstClass(c) => Some(c)
        case _                                   => None
      }

    // Unapply the symbols contained in a firstClass comment.
    def unapply(comment: Comment): Option[Seq[String]] =
      if (isFirstClass(comment)) Some(firstClassSyms(comment))
      else None
  }

  object nonEmpty {
    def unapply[X](xs: Seq[X]): Option[Seq[X]] =
      if (xs.isEmpty) None else Some(xs)
  }

  object leftMost {
    def unapply[X](content: Seq[X]): Option[(Seq[X], X)] =
      content.lastOption.map(c => content.dropRight(1) -> c)
  }

  object rightMost {
    def unapply[X](content: Seq[X]): Option[(X, Seq[X])] =
      content.headOption.map(c => c -> content.drop(1))
  }

  // Returns a new comment that prepends the "Hello" symbol to it's left symbols,
  // right symbols are unmodified
  val hello: Reduction = {
    case Arg(
          runtime,
          leftMost(lprev, Symbols(leftMost(syms, sym))),
          firstClass(rightMost("hello", rfs)),
          right: Seq[Content]
        ) =>
      val result =
        lprev ++ Seq(
          Symbols(syms),
          commentOf(Symbols(Seq("Hello", sym) ++ rfs))
        ) ++ right
      Res(runtime, result)
  }

  // throw Implementation is missing for any comment having the ??? symbol
  val missing: Reduction = {
    case Arg(r, left, comment, right)
        if isFirstClass(comment) && firstClassSyms(comment).contains("???") =>
      ???
  }

//  val bigDecimal: Reduction = {
//    case Arg(
//          runtime,
//          leftMost(lprev, Symbols(leftMost(lsyms, num))),
//          FirstClass("BigDecimal"),
//          right
//        ) =>
//    Res(runtime, )
//  }

  val loadURL: Reduction = {
    case Arg(
          runtime,
          leftMost(lprev, Symbols(leftMost(syms, sym))),
          firstClass(rightMost("loadURL", rhs)),
          right
        ) =>
      val code = Source.fromURL(sym).mkString
      val fastparse.Parsed.Success(ast: Seq[Content], _) = Parser(code)
      Res(
        runtime,
        lprev ++ Seq(Symbols(syms)) ++ ast ++
          Seq(commentOf(Symbols(rhs))) ++ right
      )
  }

  val load: Reduction = {
    case Arg(
          runtime,
          leftMost(lprev, Symbols(path :: Nil)),
          firstClass(rightMost("load", rhs)),
          right
        ) =>
      val fpath = os.Path(path, os.pwd)
      val source =
        if (fpath.toIO.exists()) os.read(fpath)
        else Source.fromURL(getClass.getResource(s"/${path}.vrv")).mkString
      val fastparse.Parsed.Success(ast: Seq[Content], _) = Parser(source)
      Res(
        runtime,
        lprev ++ ast ++
          Seq(commentOf(Symbols(rhs))) ++ right
      )
  }
}

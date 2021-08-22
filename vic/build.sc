// -*- scala -*-
import mill._, scalalib._

object vrv extends ScalaModule {
  override def scalaVersion = "2.13.6"

  override def sources   =
    T.sources(millOuterCtx.millSourcePath / "src")

  override def resources =
    T.sources(
      millOuterCtx.millSourcePath / "samples",
      millOuterCtx.millSourcePath / "stdlib"
    )

  override def ivyDeps =
    super.ivyDeps() ++ Agg(ivy"com.lihaoyi:::ammonite::2.4.0")
}

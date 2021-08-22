package vrv

final case class Runtime(reductions: Seq[Runtime.Reduction])
object Runtime {
  type Reduction = PartialFunction[Arg, Res]
  final case class Arg(
      runtime: Runtime,
      left: Seq[Content],
      comment: Comment,
      right: Seq[Content]
  )
  final case class Res(runtime: Runtime, result: Seq[Content])
}

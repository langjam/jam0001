package vrv

trait Show[T] {
  def show(content: T): String
}

object Show {
  def apply[T](content: T)(implicit pp: Show[T]): String = pp.show(content)
}

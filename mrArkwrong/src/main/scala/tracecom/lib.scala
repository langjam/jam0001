package tracecom

import zio.blocking.{Blocking, blocking}
import zio.console.*
import zio.{IO, Task, ZIO}

import java.io.IOException
import scala.io.Source


def safeZip[A, B](as: List[A], bs: List[B]): Option[List[(A, B)]] = (as, bs)  match
  case (aHead :: aTail, bHead :: bTail) => for {
    zippedTail <- safeZip(aTail, bTail)
  } yield (aHead, bHead) :: zippedTail

  case (Nil, Nil) => Some(Nil)

  case _ => None

extension (self: String)
  def indent(level: Int) = self.linesIterator.toList.map("  " * level ++ _).mkString("\n")

extension [E, A](self: Either[E, A])
  def toZio: IO[E, A] = ZIO.fromEither(self)

extension [A](self: Option[A])
  def toZio[E](error: E): IO[E, A] = ZIO.fromOption(self).asError(error)

extension [R, E, A](self: ZIO[R, E, A])
  def asError[E1](error: => E1): ZIO[R, E1, A] = self.mapError(_ => error)

def readFile(file: String): ZIO[Blocking, IOException, String] = blocking {
  val open: Task[Source] = ZIO.effect(Source.fromFile(file))
  val close = (s: Source) => ZIO.effect(s.close()).orDie
  open.bracket(source => close(source))(source => ZIO.effect(source.mkString)).refineToOrDie[IOException]
}

case class InstanceValue[F[_], A: F](value: A):
  def instance: F[A] = summon

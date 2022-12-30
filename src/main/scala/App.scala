import zio._
import zio.Console._

object App extends ZIOAppDefault {
  val program: ZIO[Console, Throwable, Int] = 
    for {
      day1 <- advent.Day01.solve
      day2 <- advent.Day02.solve

      _ <- printLine(day1)
      _ <- printLine(day2)
    } yield 0

  def run: ZIO[Environment with ZIOAppArgs with Scope,Any,Any] =
    program.provideEnvironment(ZEnvironment.apply(ConsoleLive))

}

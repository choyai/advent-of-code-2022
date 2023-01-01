import zio._
import zio.Console._

object App extends ZIOAppDefault {
  val program: ZIO[Console, Throwable, Int] = 
    for {
      day1 <- advent.Day01.solve
      day2 <- advent.Day02.solve
      day3 <- advent.Day03.solve
      day4 <- advent.Day04.solve

      _ <- printLine(s"day 1's answers: ${day1}")
      _ <- printLine(s"day 2's answers: ${day2}")
      _ <- printLine(s"day 3's answers: ${day3}")
      _ <- printLine(s"day 4's answers: ${day4}")
    } yield 0

  def run: ZIO[Environment with ZIOAppArgs with Scope,Any,Any] =
    program.provideEnvironment(ZEnvironment.apply(ConsoleLive))

}

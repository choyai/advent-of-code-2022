package advent

import zio._
import scala.util.Try

object Day03 {
  def solve: ZIO[Console, Throwable, String] =
    for {
      // read the input data from a file
      input <- readInputFile("src/main/resources/day03.txt")
      // parse the input data
      parsedInput = parseInput(input)
      // solve the first part of the problem
      part1 = solvePart1(parsedInput) // solve the second part of the problem
      part2 = solvePart2(parsedInput)
      // print the results to the console
    } yield s"$part1, $part2"

  def readInputFile(filename: String): ZIO[Any, Throwable, String] =
    // read the contents of the file and return a ZIO effect
    ZIO.fromTry(Try(scala.io.Source.fromFile(filename).mkString))

  def parseInput(input: String): Seq[String] =
    // parse the input string and return a sequence of integers
    // for day 2 this is the score for each round
    input
      .split("\n")

  def solvePart1(input: Seq[String]): Int =
    // solve the first part of the problem and return the result
    input
      .map(toCompartments)
      .map(findCommonChar)
      .map(getPriority)
      .sum

  def solvePart2(input: Seq[String]): Int =
    // solve the second part of the problem and return the result
    // input
    //   .map(calcCorrectScore)
    //   .sum
    input
      .sliding(3, 3)
      .map(findCommonChar)
      .sum

  def toCompartments(line: String): Seq[String] =
    line.splitAt(line.length() / 2).toList

  def findCommonChar(containers: Seq[String]): Char =
    print(containers)
    containers
      .head
      .filter( x => containers.forall(_.contains(x)) )
      .charAt(0)

  def getPriority(c: Char): Int =
    c.toInt match
      case x if x < 97 => x - 38
      // assume no input greater than 'z' and lower than 'A'
      case y => y - 96
}

package advent

import zio._
import scala.util.Try

object Day04 {
  def solve: ZIO[Console, Throwable, String] =
    for {
      // read the input data from a file
      input <- readInputFile("src/main/resources/day04.txt")
      // parse the input data
      parsedInput = parseInput(input)
      // solve the first part of the problem
      part1 = solvePart1(parsedInput) 
      // solve the second part of the problem
      part2 = solvePart2(parsedInput)
      // print the results to the console
    } yield s"$part1, $part2"

  def readInputFile(filename: String): ZIO[Any, Throwable, String] =
    // read the contents of the file and return a ZIO effect
    ZIO.fromTry(Try(scala.io.Source.fromFile(filename).mkString))

  def parseInput(input: String): Seq[String] =
    input
      .split("\n")

  def solvePart1(input: Seq[String]): Int =
    // solve the first part of the problem and return the result
    input
      .map(_.split(",").map(_.split("-").map(_.toInt)))
      .map(overlap)
      .sum

  def solvePart2(input: Seq[String]): Int =
    input
      .map(_.split(",").map(_.split("-").map(_.toInt)))
      .map(partialOverlap)
      .sum 

  def overlap(arr: Array[Array[Int]]): Int =
    arr match
      case Array(Array(x: Int, y: Int), Array(w: Int, z: Int)) if x <= w && y >= z => 1
      case Array(Array(x: Int, y: Int), Array(w: Int, z: Int)) if x >= w && y <= z => 1
      case _ => 0
  
  def partialOverlap(arr: Array[Array[Int]]): Int =
    arr match
      case Array(Array(x: Int, y: Int), Array(w: Int, z: Int)) if x <= w && y >= w => 1
      case Array(Array(x: Int, y: Int), Array(w: Int, z: Int)) if x >= w && x <= z => 1
      case _ => 0
    
}

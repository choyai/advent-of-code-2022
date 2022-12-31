package advent

import zio._
import scala.util.Try

object Day02 {

  enum Move(score: Int):
    def getScore: Int = score

    // c++ moment
    // saving for posterity
    // def getWinning: Move = 
    //   new Move((this.getScore % 3) + 1) 

    def getWinning: Move =
      this.match
        case Rock => Paper
        case Paper => Scissors
        case Scissors => Rock
      

    case Rock extends Move(1)
    case Paper extends Move(2)
    case Scissors extends Move(3)

  enum Outcome(code: Char):
    case Lose extends Outcome('X')
    case Draw extends Outcome('Y')
    case Win extends Outcome('Z')


  def solve: ZIO[Console, Throwable, String] =
    for {
      // read the input data from a file
      input <- readInputFile("src/main/resources/day02_input.txt")
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
    // parse the input string and return a sequence of integers
    // for day 2 this is the score for each round
    input
      .split("\n")

  def calcPoints(input: String): Int =
    val moves = input.split(" ")
    val oppMove: Move = moves.head.match {
      case "A" => Move.Rock
      case "B" => Move.Paper
      case "C" => Move.Scissors
    }
    val myMove: Move = moves.last.match {
      case "X" => Move.Rock
      case "Y" => Move.Paper
      case "Z" => Move.Scissors
    }
    myMove.getScore + victoryPoints(oppMove, myMove)

  def victoryPoints(oppMove: Move, myMove: Move): Int =
    (oppMove, myMove) match
      case (x, y) if x == y             => 3  
      case (Move.Rock, Move.Paper)      => 6
      case (Move.Rock, Move.Scissors)   => 0
      case (Move.Paper, Move.Scissors)  => 6
      case (Move.Paper, Move.Rock)      => 0
      case (Move.Scissors, Move.Rock)   => 6 
      case (Move.Scissors, Move.Paper)  => 0
    
  def calcCorrectScore(input: String): Int = 
    val moves = input.split(" ")
    val oppMove: Move = moves.head.match {
      case "A" => Move.Rock
      case "B" => Move.Paper
      case "C" => Move.Scissors
    }
    val outcome: Outcome = moves.last.match {
      case "X" => Outcome.Lose
      case "Y" => Outcome.Draw
      case "Z" => Outcome.Win
    }
    val myMove = getDesiredMove(oppMove, outcome)

    myMove.getScore + victoryPoints(oppMove, myMove)

  def getDesiredMove(oppMove: Move, outcome: Outcome): Move =
    outcome match
      case Outcome.Lose => oppMove.getWinning.getWinning
      case Outcome.Draw => oppMove
      case Outcome.Win  => oppMove.getWinning


  def solvePart1(input: Seq[String]): Int =
    // solve the first part of the problem and return the result
    input
      .map(calcPoints)
      .sum

  def solvePart2(input: Seq[String]): Int =
    // solve the second part of the problem and return the result
    input
      .map(calcCorrectScore)
      .sum
}

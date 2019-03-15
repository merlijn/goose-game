package com.github.merlijn.goose

import cats.effect.{ExitCode, IO, _}

import scala.io.StdIn
import scala.util.{Random, Try}

object Main extends IOApp {

  // some predefined values
  val boardLength = 63
  val diceSize = 6
  val bridgeStart = 6
  val bridgeDestination = 12
  val goosePositions = Set(5, 9, 14, 18, 23, 27)

  // regexes matching the user input
  val AddPlayer = "add\\s+player\\s+(\\w+)".r
  val RollDiceAndMovePlayer = "move\\s+(\\w+)".r
  val MovePlayer = "move\\s+(\\w+)\\s+(\\d),\\s+(\\d)".r

  // holds the state of the game
  case class Game(positions: Map[String, Int])

  // encapsulates a response to the user
  sealed trait Response

  case class Quit(text: String) extends Response

  case class Continue(response: String) extends Response

  def positionLabel(n: Int): String = {
    if (n == 0)
      "Start"
    else if (n >= boardLength)
      "63"
    else if (n == bridgeStart)
      "The Bridge"
    else
      n.toString
  }

  /**
    * Calculates the actual end position of a player after moving and a message explaining what happens.
    */
  def calculateMovementEffects(name: String, oldPosition: Int, dice: Int): (Int, String) = {

    val newPosition = oldPosition + dice

    if (newPosition == bridgeStart)
      (bridgeDestination, s". $name jumps to $bridgeDestination")
    else if (newPosition > boardLength) {
      val result = boardLength - (newPosition - boardLength)
      (result, s". $name bounces! $name returns to $result")
    }
    else if (goosePositions.contains(newPosition)) {
      val (pos, append) = calculateMovementEffects(name, newPosition, dice)
      val jump = newPosition + dice
      (pos, s", The Goose. $name moves again and goes to $jump" + append)
    }
    else
      (newPosition, "")
  }

  def movePlayer(state: Game)(name: String, roll1: Int, roll2: Int): (Game, Response) = {

    if (!state.positions.contains(name))
      (state, Continue(s"No such player: $name"))
    else if (roll1 < 1 && roll1 > diceSize)
      (state, Continue(s"Invalid value for dice roll: $roll1"))
    else if (roll2 < 1 || roll2 > diceSize)
      (state, Continue(s"Invalid value for dice roll: $roll2"))
    else {
      val diceRoll = roll1.toInt + roll2.toInt
      val currentPosition = state.positions(name)
      val result = currentPosition + diceRoll

      val (newPosition: Int, msgAppend: String) = calculateMovementEffects(name, currentPosition, diceRoll)

      val message = s"$name rolls $roll1, $roll2. $name moves from ${positionLabel(currentPosition)} to ${positionLabel(result)}" + msgAppend

      val updatedState = state.copy(positions = state.positions + (name -> newPosition))

      if (newPosition == boardLength)
        (updatedState, Quit(message + s". $name Wins!"))
      else
        (updatedState, Continue(message))
    }
  }

  def rollDice(): Int = Random.nextInt(diceSize) + 1

  /**
    * Updates the game state and returns a response given current game state and the user input.
    */
  def updateState(state: Game)(input: String): (Game, Response) = input match {
    case AddPlayer(name) =>

      if (state.positions.contains(name))
        (state, Continue(s"$name: already existing player"))
      else {
        val newPlayers = state.positions + (name -> 0)
        (state.copy(positions = newPlayers), Continue(response = "players: " + newPlayers.keys.mkString(", ")))
      }

    case MovePlayer(name, roll1, roll2) =>

      if (Try { roll1.toInt }.isFailure)
        (state, Continue(s"Invalid value for dice roll: $roll1"))
      else if (Try { roll2.toInt }.isFailure)
        (state, Continue(s"Invalid value for dice roll: $roll1"))
      else
        movePlayer(state)(name, roll1.toInt, roll2.toInt)

    case RollDiceAndMovePlayer(name) =>

      val roll1 = rollDice()
      val roll2 = rollDice()

      movePlayer(state)(name, roll1, roll2)

    case "quit" | "exit" =>

      (state, Quit("bye!"))

    case _ =>

      (state, Continue("Unrecoginized command"))
  }

  def readLine(): IO[String] = IO { StdIn.readLine() }

  def putLine(str: String): IO[Unit] = IO(println(str))

  /**
    * The main IO loop.
    *
    * Reads a line from the console
    */
  def loop(game: Game): IO[ExitCode] =
    IO.suspend {

      readLine().flatMap { line =>

        updateState(game)(line) match {

          case (_, Quit(response))  =>
            putLine(response)
              .flatMap(_ => IO.pure(ExitCode.Success) )

          case (updatedGame, Continue(response)) =>
            putLine(response)
              .flatMap(_ => loop(updatedGame))
        }
      }
    }

  override def run(args: List[String]): IO[ExitCode] = loop(Game(Map.empty))
}


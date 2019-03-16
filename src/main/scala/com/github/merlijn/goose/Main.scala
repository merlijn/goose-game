package com.github.merlijn.goose

import cats.effect.{ExitCode, IO, _}

import scala.io.StdIn
import scala.util.{Random, Try}

object Main extends IOApp {

  /**
    * Some fixed values
    */
  val boardLength = 63
  val diceSize = 6
  val bridgeStart = 6
  val bridgeDestination = 12
  val goosePositions = Set(5, 9, 14, 18, 23, 27)

  /**
    * Regular expressions for the users commands to the program
    */
  val AddPlayer = "add\\s+player\\s+(\\w+)".r
  val RollDiceAndMovePlayer = "move\\s+(\\w+)".r
  val MovePlayer = "move\\s+(\\w+)\\s+(\\d),\\s+(\\d)".r

  def rollRandomDice(): Int = Random.nextInt(diceSize) + 1

  /**
    * Holds the state of the game.
    *
    * Which is just a map from player name -> position
    */
  case class Game(positions: Map[String, Int], rollDice: () => Int = rollRandomDice) {

    def playerOnPosition(position: Int): Option[String] = {
      positions.collectFirst { case (player, playerPosition) if position == playerPosition => player }
    }

    def updatePosition(player: String, position: Int): Game = copy(positions = positions + (player -> position))
  }

  /**
    * This encapsulates a response to the user of the program.
    */
  sealed trait Response

  /**
    * Response in case the game is finished.
    */
  case class GameOver(message: String) extends Response

  /**
    * Response in case the game is still going on.
    */
  case class Continue(message: String) extends Response

  /**
    * String representation of a players position.
    */
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
  def calculateMovementEffects(player: String, oldPosition: Int, dice: Int): (Int, String) = {

    val newPosition = oldPosition + dice

    if (newPosition == bridgeStart)
      (bridgeDestination, s". $player jumps to $bridgeDestination")
    else if (newPosition > boardLength) {
      val result = boardLength - (newPosition - boardLength)
      (result, s". $player bounces! $player returns to $result")
    }
    else if (goosePositions.contains(newPosition)) {
      val (pos, append) = calculateMovementEffects(player, newPosition, dice)
      val jump = newPosition + dice
      (pos, s", The Goose. $player moves again and goes to $jump" + append)
    }
    else
      (newPosition, "")
  }

  /**
    * This moves a player, returning the updated state and a response message.
    */
  def movePlayer(state: Game)(player: String, roll1: Int, roll2: Int): (Game, Response) = {

    if (!state.positions.contains(player))
      (state, Continue(s"No such player: $player"))
    else if (roll1 < 1 || roll1 > diceSize)
      (state, Continue(s"Invalid value for dice roll: $roll1"))
    else if (roll2 < 1 || roll2 > diceSize)
      (state, Continue(s"Invalid value for dice roll: $roll2"))
    else {
      val diceRoll = roll1 + roll2
      val currentPosition = state.positions(player)
      val result = currentPosition + diceRoll

      // depending on where the player lands, some rules may apply, which are calculated here
      val (newPosition: Int, msgAppend: String) = calculateMovementEffects(player, currentPosition, diceRoll)

      // the start of the reply message is always the same. depending on the applied rules there is a message appended
      val message = s"$player rolls $roll1, $roll2. $player moves from ${positionLabel(currentPosition)} to ${positionLabel(result)}" + msgAppend

      if (newPosition == boardLength)
        (state.updatePosition(player, newPosition), GameOver(message + s". $player Wins!!"))
      else
        state.playerOnPosition(newPosition) match {
          case None =>
            (state.updatePosition(player, newPosition), Continue(message))
          case Some(otherPlayer) if player != otherPlayer =>
            (state
              .updatePosition(otherPlayer, currentPosition)
              .updatePosition(player, newPosition), Continue(message + s". On ${positionLabel(newPosition)} there is $otherPlayer, who returns to ${positionLabel(currentPosition)}"))
        }
    }
  }



  /**
    * Updates the game state and returns a response given current game state and the user input.
    */
  def updateState(state: Game)(input: String): (Game, Response) = input match {
    case AddPlayer(name) =>

      if (state.positions.contains(name))
        (state, Continue(s"$name: already existing player"))
      else {
        val newPlayers = state.positions + (name -> 0)
        (state.copy(positions = newPlayers), Continue(message = "players: " + newPlayers.keys.mkString(", ")))
      }

    case MovePlayer(name, roll1, roll2) =>

      if (Try { roll1.toInt }.isFailure)
        (state, Continue(s"Invalid value for dice roll: $roll1"))
      else if (Try { roll2.toInt }.isFailure)
        (state, Continue(s"Invalid value for dice roll: $roll1"))
      else
        movePlayer(state)(name, roll1.toInt, roll2.toInt)

    case RollDiceAndMovePlayer(name) =>

      val roll1 = state.rollDice()
      val roll2 = state.rollDice()

      movePlayer(state)(name, roll1, roll2)

    case "quit" | "exit" =>

      (state, GameOver("bye!"))

    case _ =>

      (state, Continue("Unrecoginized command"))
  }

  def readLine(): IO[String] = IO { StdIn.readLine() }

  def putLine(str: String): IO[Unit] = IO(println(str))

  /**
    * The main game loop.
    */
  def loop(game: Game): IO[ExitCode] =
    IO.suspend {

      readLine().flatMap { line =>

        updateState(game)(line) match {

          case (_, GameOver(response))  =>
            putLine(response)
              .flatMap(_ => IO.pure(ExitCode.Success) )

          case (updatedGame, Continue(response)) =>
            putLine(response)
              .flatMap(_ => loop(updatedGame))
        }
      }
    }

  /**
    * The run(...) is like to the main(...) method in cats-effect IOApp)
    *
    * It simply calls the game loop with an empty game to start.
    */
  override def run(args: List[String]): IO[ExitCode] = loop(Game(Map.empty))
}


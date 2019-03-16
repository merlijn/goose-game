package com.github.merlijn.goose

import cats.effect.{ExitCode, IO, _}
import com.github.merlijn.goose.Game._

import scala.io.StdIn

object Main extends IOApp {

  def readLine(): IO[String] = IO { StdIn.readLine() }

  def putLine(str: String): IO[Unit] = IO { println(str) }

  /**
    * The main game loop.
    */
  def loop(game: Game): IO[ExitCode] =
    IO.suspend {

      readLine().flatMap { line =>

        Game.updateState(game)(line) match {

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


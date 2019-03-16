package com.github.merlijn.goose

import com.github.merlijn.goose.Game._
import org.scalatest.{Matchers, WordSpec}

class GooseSpec extends WordSpec with Matchers {

  def testCase(state: Game, input: String): String =
    updateState(state)(input)._2 match {
      case GameOver(message) => message
      case Continue(message) => message
    }

  "A Goose game" should {

    "be able to add players" in {

      testCase(state = Game(Map.empty), input = "add player Pippo") shouldBe "players: Pippo"
      testCase(state = Game(Map("Pippo" -> 0)), input = "add player Pluto") shouldBe "players: Pippo, Pluto"
      testCase(state = Game(Map("Pippo" -> 0)), input = "add player Pippo") shouldBe "Pippo: already existing player"
    }

    "be able to move players" in {

      val start = Game(Map("Pippo" -> 0, "Pluto" -> 0))

      testCase(state = start, input = "move Pippo 4, 3") shouldBe "Pippo rolls 4, 3. Pippo moves from Start to 7"
      testCase(state = start, input = "move Pluto 2, 2") shouldBe "Pluto rolls 2, 2. Pluto moves from Start to 4"
      testCase(state = start.updatePosition("Pippo", 7), input = "move Pippo 2, 3") shouldBe "Pippo rolls 2, 3. Pippo moves from 7 to 12"
    }

    "be able to deal with the finish line" in {

      val start = Game(Map("Pippo" -> 60))

      testCase(state = start, input = "move Pippo 1, 2") shouldBe "Pippo rolls 1, 2. Pippo moves from 60 to 63. Pippo Wins!!"
      testCase(state = start, input = "move Pippo 3, 2") shouldBe "Pippo rolls 3, 2. Pippo moves from 60 to 63. Pippo bounces! Pippo returns to 61"
    }

    "allow the game to throw the dice" in {

      var diceCount = 0
      val diceRolls = Seq(1, 2)

      def customDiceRoll(): Int = {
        val roll = diceRolls(diceCount)
        diceCount += 1
        roll
      }

      val game = Game(Map("Pippo" -> 4), rollDice = customDiceRoll)

      testCase(state = game, input = "move Pippo") shouldBe "Pippo rolls 1, 2. Pippo moves from 4 to 7"
    }

    "be able to move players across the bridge" in {

      val start = Game(Map("Pippo" -> 4))

      testCase(state = start, input = "move Pippo 1, 1") shouldBe "Pippo rolls 1, 1. Pippo moves from 4 to The Bridge. Pippo jumps to 12"
    }

    "be able to handle a jump on the Goose spaces" in {

      testCase(Game(Map("Pippo" -> 3)), input = "move Pippo 1, 1") shouldBe "Pippo rolls 1, 1. Pippo moves from 3 to 5, The Goose. Pippo moves again and goes to 7"
      testCase(Game(Map("Pippo" -> 10)), input = "move Pippo 2, 2") shouldBe "Pippo rolls 2, 2. Pippo moves from 10 to 14, The Goose. Pippo moves again and goes to 18, The Goose. Pippo moves again and goes to 22"
    }

    "be able to return another player to a players previous position" in {

      testCase(Game(Map("Pippo" -> 15, "Pluto" -> 17)), input = "move Pippo 1, 1") shouldBe "Pippo rolls 1, 1. Pippo moves from 15 to 17. On 17 there is Pluto, who returns to 15"
    }
  }
}

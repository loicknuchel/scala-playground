package playground.fpToTheMax

import org.scalatest.{FunSpec, Matchers}

class MainSpec extends FunSpec with Matchers {
  describe("Main") {
    val programs = Seq[Array[String] => Unit](
      Step7.main,
      Step6.main,
      Step5.main,
      Step4.main,
      Step3.main,
      Step2.main,
      Step1.main,
      Step0.main,
      Main.main
    )
    it("should work as expected") {
      programs.map { main =>
        scala.util.Random.setSeed(0)
        val in = Seq("jean", "2", "y", "4", "n")
        ConsoleHelper.eval(main, in).split("\n") shouldBe Seq(
          "What is your name?",
          "Hello, jean, welcome to the game!",
          "Dear jean, please guess a number from 1 to 5:",
          "You guessed wrong, jean! The number was: 1",
          "Do you want to continue, jean?",
          "Dear jean, please guess a number from 1 to 5:",
          "You guessed right, jean!",
          "Do you want to continue, jean?")
      }
    }
  }
  describe("Step0") {
    val programs = Seq[Array[String] => Unit](
      Step0.main
    )
    it("should fail on invalid guess") {
      programs.map { main =>
        a[NumberFormatException] should be thrownBy ConsoleHelper.eval(main, Seq("jean", "invalid", "n"))
      }
    }
    it("should fail on invalid exit answer") {
      programs.map { main =>
        a[MatchError] should be thrownBy ConsoleHelper.eval(main, Seq("jean", "1", "invalid", "n"))
      }
    }
  }
  describe("Others") {
    val programs = Seq[Array[String] => Unit](
      Step7.main,
      Step6.main,
      Step5.main,
      Step4.main,
      Step3.main,
      Step2.main,
      Step1.main,
      Main.main
    )
    it("should fail on invalid guess") {
      programs.map { main =>
        scala.util.Random.setSeed(0)
        ConsoleHelper.eval(main, Seq("jean", "invalid", "n")).split("\n") shouldBe Seq(
          "What is your name?",
          "Hello, jean, welcome to the game!",
          "Dear jean, please guess a number from 1 to 5:",
          "You did not enter a number",
          "Do you want to continue, jean?")
      }
    }
    it("should fail on invalid exit answer") {
      programs.map { main =>
        scala.util.Random.setSeed(0)
        ConsoleHelper.eval(main, Seq("jean", "1", "invalid", "n")).split("\n") shouldBe Seq(
          "What is your name?",
          "Hello, jean, welcome to the game!",
          "Dear jean, please guess a number from 1 to 5:",
          "You guessed right, jean!",
          "Do you want to continue, jean?",
          "Do you want to continue, jean?")
      }
    }
  }
}

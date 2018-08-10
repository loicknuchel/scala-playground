package playground.fpToTheMax

import scala.io.StdIn.readLine
import scala.util.Try

/**
  * Remove loops to prepare IO refactoring (no more variables also \o/)
  */
object Step2 {
  def parseInt(in: String): Option[Int] = Try(in.toInt).toOption

  def main(args: Array[String]): Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    gameLoop(name)
  }

  def gameLoop(name: String): Unit = {
    val num = scala.util.Random.nextInt(5) + 1

    println("Dear " + name + ", please guess a number from 1 to 5:")

    parseInt(readLine()) match {
      case None => println("You did not enter a number")
      case Some(guess) =>
        if (guess == num) println("You guessed right, " + name + "!")
        else println("You guessed wrong, " + name + "! The number was: " + num)
    }

    val cont = checkContinue(name)
    if (cont) gameLoop(name) else ()
  }

  def checkContinue(name: String): Boolean = {
    println("Do you want to continue, " + name + "?")

    readLine().toLowerCase match {
      case "y" => true
      case "n" => false
      case _ => checkContinue(name)
    }
  }
}

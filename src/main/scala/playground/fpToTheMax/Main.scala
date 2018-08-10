package playground.fpToTheMax

import scala.io.StdIn.readLine
import scala.util.Try

/**
  * Step0: Imperative code that do side effects and throw errors
  * Step1: Make the program safe and not throw errors
  */
object Main {
  def parseInt(in: String): Option[Int] = Try(in.toInt).toOption

  def main(args: Array[String]): Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number from 1 to 5:")

      parseInt(readLine()) match {
        case None => println("You did not enter a number")
        case Some(guess) =>
          if (guess == num) println("You guessed right, " + name + "!")
          else println("You guessed wrong, " + name + "! The number was: " + num)
      }

      var cont = true

      while (cont) {
        cont = false

        println("Do you want to continue, " + name + "?")

        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _ => cont = true
        }
      }
    }
  }
}

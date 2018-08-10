package playground.fpToTheMax

import scala.io.StdIn.readLine
import scala.util.Try

/**
  * Use IO to have a pure program
  */
object Step3 {

  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  def putStrLn(line: => String): IO[Unit] = IO(() => println(line))

  def getStrLn: IO[String] = IO(() => readLine())

  def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))

  def parseInt(in: String): Option[Int] = Try(in.toInt).toOption

  def main(args: Array[String]): Unit = mainIO.unsafeRun()

  def mainIO: IO[Unit] =
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()

  def gameLoop(name: String): IO[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn
      _ <- parseInt(input) match {
        case None => putStrLn("You did not enter a number")
        case Some(guess) =>
          if (guess == num) putStrLn("You guessed right, " + name + "!")
          else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
      }
      cont <- checkContinue(name)
      _ <- if (cont) gameLoop(name) else IO.point(())
    } yield ()

  def checkContinue(name: String): IO[Boolean] =
    for {
      _ <- putStrLn("Do you want to continue, " + name + "?")
      input <- getStrLn
      cont <- input.toLowerCase match {
        case "y" => IO.point(true)
        case "n" => IO.point(false)
        case _ => checkContinue(name)
      }
    } yield cont
}

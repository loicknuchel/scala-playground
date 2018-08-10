package playground.fpToTheMax

import scala.io.StdIn.readLine
import scala.language.higherKinds
import scala.util.Try

/**
  * Create a Program runner so we can abstract the IO monad and explicitly add dependencies
  */
object Step4 {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]

    def chain[A, B](m: F[A], f: A => F[B]): F[B]

    def map[A, B](m: F[A], f: A => B): F[B]
  }

  implicit class ProgramSyntax[F[_], A](m: F[A]) {
    def map[B](f: A => B)(implicit p: Program[F]): F[B] = p.map(m, f)

    def flatMap[B](f: A => F[B])(implicit p: Program[F]): F[B] = p.chain(m, f)
  }

  trait Console[F[_]] {
    def putStrLn(line: => String): F[Unit]

    def getStrLn: F[String]
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      override def finish[A](a: => A): IO[A] = IO.point(a)

      override def chain[A, B](m: IO[A], f: A => IO[B]): IO[B] = m.flatMap(f)

      override def map[A, B](m: IO[A], f: A => B): IO[B] = m.map(f)
    }

    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      def putStrLn(line: => String): IO[Unit] = IO(() => println(line))

      def getStrLn: IO[String] = IO(() => readLine())
    }
    implicit val RandomIO: Random[IO] = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }
  }

  def finish[F[_], A](a: => A)(implicit p: Program[F]): F[A] = p.finish(a)

  def putStrLn[F[_]](line: => String)(implicit c: Console[F]): F[Unit] = c.putStrLn(line)

  def getStrLn[F[_]](implicit c: Console[F]): F[String] = c.getStrLn

  def nextInt[F[_]](upper: Int)(implicit r: Random[F]): F[Int] = r.nextInt(upper)

  def parseInt(in: String): Option[Int] = Try(in.toInt).toOption

  def main(args: Array[String]): Unit = mainIO.unsafeRun()

  def mainIO: IO[Unit] = mainProgram[IO]

  def mainProgram[F[_]](implicit p: Program[F], c: Console[F], r: Random[F]): F[Unit] =
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()

  def gameLoop[F[_]](name: String)(implicit p: Program[F], c: Console[F], r: Random[F]): F[Unit] =
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
      _ <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def checkContinue[F[_]](name: String)(implicit p: Program[F], c: Console[F]): F[Boolean] =
    for {
      _ <- putStrLn("Do you want to continue, " + name + "?")
      input <- getStrLn
      cont <- input.toLowerCase match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _ => checkContinue(name)
      }
    } yield cont
}

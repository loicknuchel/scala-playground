package playground.fpToTheMax

import scala.io.StdIn.readLine
import scala.language.higherKinds
import scala.util.Try

/**
  * Add a TestIO Monad to mock system and easily test the program
  */
object Step6 {

  trait Program[F[_]] {
    def finish[A](a: => A): F[A]

    def chain[A, B](m: F[A], f: A => F[B]): F[B]

    def map[A, B](m: F[A], f: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit p: Program[F]): Program[F] = p
  }

  implicit class ProgramSyntax[F[_], A](m: F[A]) {
    def map[B](f: A => B)(implicit p: Program[F]): F[B] = p.map(m, f)

    def flatMap[B](f: A => F[B])(implicit p: Program[F]): F[B] = p.chain(m, f)
  }

  trait Console[F[_]] {
    def putStrLn(line: => String): F[Unit]

    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit c: Console[F]): Console[F] = c
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit r: Random[F]): Random[F] = r
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

  case class TestData(input: List[String], output: List[String], nums: List[Int]) {
    def putStrLn(line: => String): (TestData, Unit) =
      (copy(output = line :: output), ())

    def getStrLn: (TestData, String) =
      (copy(input = input.tail), input.head)

    def nextInt(upper: Int): (TestData, Int) =
      (copy(nums = nums.tail), nums.head)

    def showResults: String = output.reverse.mkString("\n")
  }

  case class TestIO[A](run: TestData => (TestData, A)) {
    def map[B](f: A => B): TestIO[B] =
      TestIO(td => run(td) match {
        case (d, a) => (d, f(a))
      })

    def flatMap[B](f: A => TestIO[B]): TestIO[B] =
      TestIO(td => run(td) match {
        case (d, a) => f(a).run(d)
      })

    def eval(d: TestData): TestData = run(d)._1
  }

  object TestIO {
    def point[A](a: => A): TestIO[A] = TestIO(d => (d, a))

    implicit val ProgramTestIO: Program[TestIO] = new Program[TestIO] {
      override def finish[A](a: => A): TestIO[A] = TestIO.point(a)

      override def chain[A, B](m: TestIO[A], f: A => TestIO[B]): TestIO[B] = m.flatMap(f)

      override def map[A, B](m: TestIO[A], f: A => B): TestIO[B] = m.map(f)
    }

    implicit val ConsoleTestIO: Console[TestIO] = new Console[TestIO] {
      def putStrLn(line: => String): TestIO[Unit] = TestIO(d => d.putStrLn(line))

      def getStrLn: TestIO[String] = TestIO(d => d.getStrLn)
    }
    implicit val RandomTestIO: Random[TestIO] = new Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] = TestIO(d => d.nextInt(upper))
    }
  }

  def finish[F[_] : Program, A](a: => A): F[A] = Program[F].finish(a)

  def putStrLn[F[_] : Console](line: => String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn

  def nextInt[F[_] : Random](upper: Int): F[Int] = Random[F].nextInt(upper)

  def parseInt(in: String): Option[Int] = Try(in.toInt).toOption

  def main(args: Array[String]): Unit = {
    mainIO.unsafeRun()
    /*val res = mainTestIO.eval(TestData(
      input = List("loic", "2", "n"),
      output = List(),
      nums = List(1)
    ))
    println("Result :\n" + res.showResults)*/
  }

  def mainIO: IO[Unit] = mainProgram[IO]

  def mainTestIO: TestIO[Unit] = mainProgram[TestIO]

  def mainProgram[F[_] : Program : Console : Random]: F[Unit] =
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()

  def gameLoop[F[_] : Program : Console : Random](name: String): F[Unit] =
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

  def checkContinue[F[_] : Program : Console](name: String): F[Boolean] =
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

package playground.fpToTheMax

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

object ConsoleHelper {
  def eval[A](thunk: Array[String] => Unit, in: Seq[String] = Seq(), args: Array[String] = Array()): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(out)) {
      Console.withIn(new ByteArrayInputStream(in.mkString("\n").getBytes(StandardCharsets.UTF_8))) {
        thunk(args)
      }
    }
    out.toString
  }
}

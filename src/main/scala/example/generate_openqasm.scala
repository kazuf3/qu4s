package example

import com.qu4s.core._

object Grover2 {
  implicit val c = new Circuit

  val q0 = new Wire("q0")
  val q1 = new Wire("q1")
  val q2 = new Wire("q2")

  // init
  H(q0)
  H(q1)
  H(q2)

  // grover(2)
  // oracle
  X(q0)
  H(q1)
  CCX(q0, q1, q2)
  X(q0)
  H(q2)

  // diffusion
  H(q0)
  H(q1)
  H(q2)
  X(q0)
  X(q1)
  X(q2)
  H(q2)

  CCX(q0, q1, q2)
  H(q2)
  X(q0)
  X(q1)
  X(q2)
  H(q0)
  H(q1)
  H(q2)

}
object GenerateOpenQASM extends App {
  Console.println(OpenQASM(Grover2.c))
}

object WriteOpenQASM extends App {
  import java.io.{PrintWriter,File}
  args.lift(1) match{
    case None => Console.println("Type file name as args")
    case Some(a) => {
      val pw = new PrintWriter(new File(args(1)))
      pw.write(OpenQASM(Grover2.c))
      pw.close()
    }
  }
}

---
title: "Introduction to Qu4s"
date: 2022-09-30T16:11:54-07:00
draft: false
---
# What is Qu4s

Qu4s is quantum circuit design library which generates OpenQASM 2.0, so that
users can use qisjob to post to IBM Quantum.

# Why Qu4s

Many of quantum circuit design tools are written in Python. As an non-python
application engineer, when we want to include quantum computation as part of the
app, we have to use Python only for that purpose. Qu4s is written in Scala and
serves well in most of JVM languages. An app in Scala can modify the quantum
circuit during program execution.

# Example

The following example can be also found [here](https://github.com/kazuf3/qu4s/blob/master/src/main/scala/example/generate_openqasm.scala).

```
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
```

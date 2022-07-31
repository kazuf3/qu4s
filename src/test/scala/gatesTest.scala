package com.qu4s.gates
import com.qu4s.gates.{Gate => G}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GatesCompilationTestSpec extends AnyFlatSpec with Matchers {
  implicit val c = new Circuit
  "H gate" should "compile" in {
    val q0 = new Wire("q0")
    val q1 = new Wire("q1")
    val q2 = new Wire("q2")
    Gate.step(
      Seq(
        Gate.h(q0),
        Gate.h(q1),
        Gate.h(q2)
      )
    )
  }
  "H with CX gate" should "compile" in {
    val q0 = new Wire("q0")
    val q1 = new Wire("q1")

   Gate.h(q0)
    Gate.cx(q0, q1)
  }
  "Grover" should "compile" in {
    val q0 = new Wire("q0")
    val q1 = new Wire("q1")
    val q2 = new Wire("q2")

    def init(q0: Wire, q1: Wire, q2: Wire):Unit = {
      G h q0
      G h q1
      G h q2
    }


    def grover2(q0: Wire, q1: Wire, q2: Wire) = {
      G step Seq(G x q0, G h q1)
      G ccx (q0, q1, q2)
      G step Seq(G x q0, G h q2)
      G step Seq(G h q0, G h q1, G h q2)
      G step Seq(G x q0, G x q1, G x q2)
      G h q2

      G ccx (q0, q1, q2)
      G h q2
      G step Seq(G x q0, G x q1, G x q2)
      G step Seq(G h q0, G h q1, G h q2)
    }
    init(q0, q1, q2)
    grover2(q0, q1, q2)
  }
}
class CircuitGenerationTestSpec extends AnyFlatSpec with Matchers {
  "H gate" should "generate right circuit" in {
    implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")

    G h q0
    G cx (q0, q1)

    c.toQASM shouldEqual
    """qubits 2

prep_z q[0:1]

H q[0]
CNOT q[0],q[1]

measure_all
"""
  }
  "QFT" should "generate QFT circuit" in {
    implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")
    val q2 = new Wire("2")

    G h q0
    G cu1 (q1, q0, 0.5)
    G h q1
    G cu1 (q2, q0, 0.25)
    G cu1 (q2, q1, 0.5)
    G h q2

    c.toQASM shouldEqual
    """qubits 3

prep_z q[0:2]

H q[0]
cu1(pi*0.5) q[1],q[0]
H q[1]
cu1(pi*0.25) q[2],q[0]
cu1(pi*0.5) q[2],q[1]
H q[2]

measure_all
"""
  }
}

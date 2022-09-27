package com.qu4s.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.qu4s.core._

class GatesCompilationTestSpec extends AnyFlatSpec with Matchers {
  implicit val c = new Circuit
  "H gate" should "compile" in {
    val q0 = new Wire("q0")
    val q1 = new Wire("q1")
    val q2 = new Wire("q2")
    H(q0)
    H(q1)
    H(q2)
  }
  "H with CX gate" should "compile" in {
    val q0 = new Wire("q0")
    val q1 = new Wire("q1")

    H(q0)
    CX(q0, q1)
  }
  "Grover" should "compile" in {
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
}
class CircuitGenerationTestSpec extends AnyFlatSpec with Matchers {
  "H gate" should "generate right cQASM circuit" in {
    implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")

    H(q0)
    CX(q0, q1)

    CQASM(c) shouldEqual
    """qubits 2

prep_z q[0:1]

H q[0]
CNOT q[0],q[1]

measure_all
"""
  }
  "H gate" should "generate right OpenQASM circuit" in {
  implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")

    H(q0)
    CX(q0, q1)

    OpenQASM(c) shouldEqual(
      """OPENQASM 2.0;
include "qelib1.inc";

qreg q0[1];
qreg q1[1];
creg c0[1];
creg c1[1];

h q[0];
cx q[0],q[1];

measure q0 -> c0;
measure q1 -> c1;"""
      )
  }

  "QFT" should "generate right cQASM QFT circuit" in {
    implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")
    val q2 = new Wire("2")

    H (q0)
    Cu1 (q1, q0, 0.5)
    H(q1)
    Cu1 (q2, q0, 0.25)
    Cu1 (q2, q1, 0.5)
    H(q2)

    CQASM(c) shouldEqual
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
  "QFT" should "generate right OpenQASM QFT circuit" in {
    implicit val c = new Circuit
    val q0 = new Wire("0")
    val q1 = new Wire("1")
    val q2 = new Wire("2")

    H (q0)
    Cu1 (q1, q0, 0.5)
    H (q1)
    Cu1 (q2, q0, 0.25)
    Cu1 (q2, q1, 0.5)
    H (q2)

    OpenQASM(c) shouldEqual(
      """OPENQASM 2.0;
include "qelib1.inc";

qreg q0[1];
qreg q1[1];
qreg q2[1];
creg c0[1];
creg c1[1];
creg c2[1];

h q[0];
cu1(pi*0.5) q[1],q[0];
h q[1];
cu1(pi*0.25) q[2],q[0];
cu1(pi*0.5) q[2],q[1];
h q[2];

measure q0 -> c0;
measure q1 -> c1;
measure q2 -> c2;"""
    )
  }
}

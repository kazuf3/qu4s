package com.qu4s.core

/**
  * cQASM converter object is to transpile Qu4s Circuit into cQASM as String.
  * ==Usage==
  *
  * {{{
  * implicit val c = new Circuit
  * val q0 = new Wire("q0")
  * H(q0)
  * CQASM(c)
  * }}}
  */
object CQASM{
  /**
    * Transpiles given Qu4s circuit into cQASM.
    * @param c
    * @return
    */
  def apply(c: Circuit):String = {
    val qubits = s"qubits ${c.wires.size.toString}\n\n"
    val prep = s"prep_z q[0:${c.wires.size-1}]\n\n"
    qubits ++ prep ++ c.gates.foldLeft("")((a,b) => a++ b.toCQASM() ++ "\n") ++ "\nmeasure_all\n"
  }
  /**
    * GateWithOpenQASM provides verbal transpilation of each quantum gate.
    * @param g
    */
  implicit class GateWithOpenQASM(g:Gate){
    /**
      * The transpilation function which will be inserted as member method of [[com.qu4s.core.Gate]].
      *
      * @return Transpiled cQASM as String
      */
    def toCQASM(): String = g match {
      case H(input) => s"H q[${input.name}]"
      case X(input) => s"Z q[${input.name}]"
      case CX(a0, a1) => s"CNOT q[${a0.name}],q[${a1.name}]"
      case CCX(control1, control2, target) => s"Toffoli q[${control1.name}], q[${control2.name}], q[$target.name]"
      case Cu1(control, target, theta) => s"cu1(pi*$theta) q[${control.name}],q[${target.name}]"
    }
  }
}

/**
  * OpenQASM converter object is to transpile Qu4s Circuit into OpenQASM 2.0 as
  * String. In order to run OpenQASM2.0 on IBM Quantum, it uses IBM Quantum
  * default library "qelib1.inc".
  * ==Usage==
  *
  * {{{
  * implicit val c = new Circuit
  * val q0 = new Wire("q0")
  * H(q0)
  * OpenQASM(c)
  * }}}
  */
object OpenQASM{
  def apply(c: Circuit):String = {
    val head = """OPENQASM 2.0;
include "qelib1.inc";

"""
    val qubits = for (name <- c.wires) yield s"qreg q${name}[1];"
    val constants = for(i <- 0 to c.wires.size - 1) yield s"creg c${i.toString}[1];"
    val measure = for ((q,c) <- c.wires.zipWithIndex) yield s"measure q${q} -> c${c};"
    head ++ {
      qubits  ++ constants ++ Seq("") ++ c.gates.map(_.toOpenQASM()) ++ Seq("") ++ measure}.mkString("\n")
  }
  implicit class GateWithOpenQASM(g: Gate) {
    def toOpenQASM(): String = g match {
      case H(input) => s"h q[${input.name}];"
      case X(input) => s"x q[${input.name}];"
      case CX(a0, a1) => s"cx q[${a0.name}],q[${a1.name}];"
      case CCX(control1, control2, target) => s"ccx q[${control1.name}], q[${control2.name}], q[$target.name];"
      case Cu1(control, target, theta) => s"cu1(pi*$theta) q[${control.name}],q[${target.name}];"

    }
  }
}


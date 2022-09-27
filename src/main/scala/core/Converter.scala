package com.qu4s.core

object CQASM{
  def apply(c: Circuit):String = {
    val qubits = s"qubits ${c.wires.size.toString}\n\n"
    val prep = s"prep_z q[0:${c.wires.size-1}]\n\n"
    qubits ++ prep ++ c.gates.foldLeft("")((a,b) => a++ b.toCQASM() ++ "\n") ++ "\nmeasure_all\n"
  }
  implicit class GateWithOpenQASM(g:Gate){
    def toCQASM(): String = g match {
      case H(input) => s"H q[${input.name}]"
      case X(input) => s"Z q[${input.name}]"
      case CX(a0, a1) => s"CNOT q[${a0.name}],q[${a1.name}]"
      case CCX(control1, control2, target) => s"Toffoli q[${control1.name}], q[${control2.name}], q[$target.name]"
      case Cu1(control, target, theta) => s"cu1(pi*$theta) q[${control.name}],q[${target.name}]"
    }
  }
}
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


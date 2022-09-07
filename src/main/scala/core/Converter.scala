package com.qu4s.core

object CQASM{
  def apply(c: Circuit):String = {
    val qubits = s"qubits ${c.wires.size.toString}\n\n"
    val prep = s"prep_z q[0:${c.wires.size-1}]\n\n"
    qubits ++ prep ++ c.gates.foldLeft("")((a,b) => a++ b.toCQASM() ++ "\n") ++ "\nmeasure_all\n"
  }
}
object OpenQASM{
  def apply(c: Circuit):String = {
    val head = """OPENQASM 2.0;
include "qelib1.inc";

"""
    val qubits = s"qreg q[${c.wires.size.toString}];\n"
    val constants = s"creg c[${c.wires.size.toString}];\n\n"
    val measure = "\nmeasure q -> c;\n"
    head ++ qubits ++ constants ++ c.gates.foldLeft("")((a,b)=>a++ b.toOpenQASM() ++ "\n") ++ measure
  }
}


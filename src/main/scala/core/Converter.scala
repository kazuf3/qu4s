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
    val qubits = for (name <- c.wires) yield s"qreg ${name}[1];"
    val constants = for(i <- 0 to c.wires.size - 1) yield s"creg c${i.toString}[1];"
    val measure = for ((q,c) <- c.wires.zipWithIndex) yield s"measure ${q} -> c${c};"
    head ++ {
      qubits  ++ constants ++ Seq("") ++ c.gates.map(_.toOpenQASM()) ++ Seq("") ++ measure}.mkString("\n")
  }
}


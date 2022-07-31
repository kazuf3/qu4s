package com.qu4s.gates

class Wire(val name:String)(implicit c: Circuit){
  c.wires = c.wires.appended(name)
}

object Gate{
  def h(input: Wire)(implicit c: Circuit): Unit = {
    c.steps = c.steps.appended("H q[" ++ input.name ++ "]\n")
  }
  def x(input: Wire)(implicit c: Circuit): Unit = {}
  def cx(a0: Wire, a1: Wire)(implicit c: Circuit): Unit = {
    c.steps = c.steps.appended("CNOT q[" ++ a0.name ++ "],q[" ++ a1.name ++ "]\n")
  }
  def cu(input: Wire, theta:Double, phi:Double, lambda:Double)(implicit c: Circuit): Unit = {}
  def cu1(control: Wire, target: Wire, theta: Double)(implicit c: Circuit): Unit = {
    c.steps = c.steps.appended(s"cu1(pi*$theta) q[${control.name}],q[${target.name}]\n")
  }

  def p(input: Wire, lambda: Double)(implicit c: Circuit):Unit = {}
  def I(input: Wire)(implicit c: Circuit): Unit = {}
  def X(input: Wire)(implicit c: Circuit): Unit = {}
  def Y(input: Wire)(implicit c: Circuit): Unit = {}

  def step(seq: Seq[Unit])(implicit c: Circuit): Unit = {}
  def ccx(a0: Wire, a1:Wire, a2: Wire)(implicit c: Circuit): Unit = {}
}

class Circuit{
  var wires: Seq[String] = Seq()
  var steps: Seq[String] = Seq()
  def toQASM() = {

    val qubits = "qubits " ++ wires.size.toString ++ "\n\n"
    val prep = "prep_z q[0:" ++ (wires.size - 1).toString ++ "]\n\n"

    qubits ++ prep ++ steps.fold("")((a,b) => a++b) ++ "\nmeasure_all\n"
  }
}

package com.qu4s.core

/**
  * A wire is used for declare a quantum bit.
  *
  * @constructor Creates a quantum bit with its name. It will also adds the wire
  * into given circuit as [[com.qu4s.core.Circuit.wires]].
  * @param name
  * @param c which quantum circuit the quantum bit belongs to
  */
case class Wire(val name:String)(implicit c: Circuit){
  c.wires = c.wires.appended(name)
}

/**
  * A gate is an abstract class which should be superclass of all the quantum gates.
  */
abstract trait Gate {
}

/**
  * H gate
  * @constructor Creates a H gate which connects to given quantum bit in given circuit.
  * @param input
  * @param c
  */
case class H private (input: Wire)(implicit c: Circuit) extends Gate{
  c.gates = c.gates:+this
}

/**
  * X gate
  * @constructor Creates a H gate which connects to given quantum bit in given circuit.
  * @param input
  * @param c
  */
case class X private (input: Wire)(implicit c: Circuit) extends Gate{
  c.gates = c.gates:+this
}

/**
  * CNOT gate
  *
  * @param a0
  * @param a1
  * @param c
  */
case class CX(a0:Wire, a1:Wire)(implicit c: Circuit) extends Gate{
  c.gates = c.gates:+this
}

/**
  * CCNOT gate, a.k.a Toffoli gate
  *
  * @param control1
  * @param control2
  * @param target
  * @param c
  */
case class CCX(control1: Wire, control2: Wire, target: Wire)
              (implicit c: Circuit)extends Gate {
  c.gates = c.gates:+this
}

/**
  * Controled-U1 gate
  *
  * @param control
  * @param target
  * @param theta
  * @param c
  */
case class Cu1(control: Wire, target: Wire, theta: Double)(implicit c: Circuit) extends Gate{
  c.gates = c.gates:+this
}

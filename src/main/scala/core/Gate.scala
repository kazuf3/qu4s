package com.qu4s.core

abstract class AbsWire {
  val name: String
}
/** A wire is used for declare a quantum bit.
  *
  * @constructor
  *   Creates a quantum bit with its name. It will also adds the wire into given circuit as
  *   [[com.qu4s.core.Circuit.wires]].
  * @param name
  * @param c
  *   which quantum circuit the quantum bit belongs to
  */
case class Wire(val name: String)(implicit c: Circuit) extends AbsWire{
  c.wires = c.wires.contains(name) match {
    case true => c.wires
    case false => c.wires.appended(name)
  }
}

case class Wire1()(implicit c: Circuit) extends AbsWire{
  val name = "static1"
  c.wires = c.wires.contains("static1") match {
    case true => c.wires
    case false => c.wires.appended(name)
  }
}
case class Wire0()(implicit c:Circuit) extends AbsWire{
  val name = "static0"
  c.wires = c.wires.contains("static0") match {
    case true => c.wires
    case false => c.wires.appended(name)
  }
}
/** A gate is an abstract class which should be superclass of all the quantum gates.
  */
abstract trait Gate {}

/** H gate
  * @constructor
  *   Creates a H gate which connects to given quantum bit in given circuit.
  * @param input
  * @param c
  */
case class H(val input: Wire)(implicit c: Circuit) extends Gate {
  c.gates = c.gates :+ this
}

/** X gate
  * @constructor
  *   Creates a H gate which connects to given quantum bit in given circuit.
  * @param input
  * @param c
  */
case class X(val input: AbsWire)(implicit c: Circuit) extends Gate {
  c.gates = c.gates :+ this
}

/** CNOT gate
  *
  * @param a0
  * @param a1
  * @param c
  */
case class CX(val a0: AbsWire, val a1: Wire)(implicit c: Circuit) extends Gate {
  c.gates = c.gates :+ this
}

/** CCNOT gate, a.k.a Toffoli gate
  *
  * @param control1
  * @param control2
  * @param target
  * @param c
  */
case class CCX(val control1: AbsWire, val control2: AbsWire, val target: Wire)(implicit c: Circuit) extends Gate {
  c.gates = c.gates :+ this
}

/** Controled-U1 gate
  *
  * @param control
  * @param target
  * @param theta
  * @param c
  */
case class Cu1(val control: AbsWire, val target: Wire, val theta: Double)(implicit c: Circuit) extends Gate {
  c.gates = c.gates :+ this
}

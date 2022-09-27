package com.qu4s.core

class Wire(val name:String)(implicit c: Circuit){
  c.wires = c.wires.appended(name)
}

abstract trait Gate {
}

case class H private (input: Wire)(implicit c: Circuit) extends Gate{
}
object H {
  def apply(input: Wire)(implicit c: Circuit):H = {
    val h = new H(input)
    c.gates = c.gates:+h
    h
  }
}
case class X(input: Wire)(implicit c: Circuit) extends Gate{
}
object X {
  def apply(input: Wire)(implicit c: Circuit):X = {
    val x = new X(input)
    c.gates = c.gates:+x
    x
  }
}
case class CX(a0:Wire, a1:Wire)(implicit c: Circuit) extends Gate{
}
object CX{
  def apply(a0:Wire, a1:Wire)(implicit c:Circuit):CX = {
    val cx = new CX(a0, a1)
    c.gates = c.gates:+cx
    cx
  }
}
case class CCX(control1: Wire, control2: Wire, target: Wire)
    (implicit c: Circuit)extends Gate {
}
object CCX {
  def apply(control1: Wire, control2: Wire, target: Wire)(implicit c:Circuit): CCX = {
    val ccx = new CCX(control1, control2, target)
    c.gates = c.gates:+ccx
    ccx
  }
}
case class Cu1(control: Wire, target: Wire, theta: Double)(implicit c: Circuit) extends Gate{
}
object Cu1 {
  def apply(control: Wire, target: Wire, theta: Double)(implicit c: Circuit):Cu1 = {
    val cu1 = new Cu1(control: Wire, target: Wire, theta: Double)
    c.gates = c.gates:+cu1
    cu1
  }
}

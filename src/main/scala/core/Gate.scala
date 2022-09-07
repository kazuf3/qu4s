package com.qu4s.core

class Wire(val name:String)(implicit c: Circuit){
  c.wires = c.wires.appended(name)
}

abstract trait Gate {
  def toCQASM(): String
  def toOpenQASM(): String
}

class H(input: Wire) extends Gate{
  override def toCQASM():String = s"H q[${input.name}]"
  override def toOpenQASM():String = s"h q[${input.name}];"
}
object H {
  def apply(input: Wire)(implicit c: Circuit):Unit = {
    val h = new H(input)
    c.gates = c.gates:+h
  }
}
class X(input: Wire) extends Gate{
  override def toCQASM():String = s"X q[${input.name}]"
  override def toOpenQASM():String = s"X q[${input.name}];"
}
object X {
  def apply(input: Wire)(implicit c: Circuit):Unit = {
    val x = new X(input)
    c.gates = c.gates:+x
  }
}
class CX(a0:Wire, a1:Wire) extends Gate{
  override def toCQASM():String = s"CNOT q[${a0.name}],q[${a1.name}]"
  override def toOpenQASM():String = s"cx q[${a0.name}],q[${a1.name}];"
}
object CX{
  def apply(a0:Wire, a1:Wire)(implicit c:Circuit):Unit = {
    val cx = new CX(a0, a1)
    c.gates = c.gates:+cx
  }
}
class CCX(control1: Wire, control2: Wire, target: Wire) extends Gate {
  override def toCQASM():String = s"Toffoli q[${control1.name}], q[${control2.name}], q[$target.name]"
  override def toOpenQASM(): String = s"ccx q[${control1.name}], q[${control2.name}], q[$target.name];"
}
object CCX {
  def apply(control1: Wire, control2: Wire, target: Wire)(implicit c:Circuit): Unit = {
    val ccx = new CCX(control1, control2, target)
    c.gates = c.gates:+ccx
  }
}
class Cu1(control: Wire, target: Wire, theta: Double) extends Gate{
  override def toCQASM(): String = s"cu1(pi*$theta) q[${control.name}],q[${target.name}]"
  override def toOpenQASM(): String = s"cu1(pi*$theta) q[${control.name}],q[${target.name}];"
}
object Cu1 {
  def apply(control: Wire, target: Wire, theta: Double)(implicit c: Circuit):Unit = {
    val cu1 = new Cu1(control: Wire, target: Wire, theta: Double)
    c.gates = c.gates:+cu1
  }
}

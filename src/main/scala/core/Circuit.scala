package com.qu4s.core

/**
  * A Circuit is a data structure which stores sequences of gates and quantum
  * bits. Qu4s will generate all the constant bits corresponds to each quantum
  * bit, and measure all of them in the end.
  */
class Circuit{
  var gates: Seq[Gate] = Seq()
  var wires: Seq[String] = Seq()
}

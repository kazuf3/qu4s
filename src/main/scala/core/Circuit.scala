package com.qu4s.core

import com.qu4s.core._

/** A Circuit is a data structure which stores sequences of gates and quantum bits. Qu4s will generate all the constant
  * bits corresponds to each quantum bit, and measure all of them in the end.
  */
class Circuit {
  var gates: Seq[Gate] = Seq()
  var wires: Seq[String] = Seq()
  var accCount: Int = 0
  def newAccWire(): Wire = {
    val newWireName = "acc" ++ accCount.toString()
    accCount = accCount + 1
    Wire(newWireName)(this)
  }
}

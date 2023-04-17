package com.qu4s.BooleanLogic

import com.qu4s.core._

abstract class LogicWire {
  val s: String
  def getLiteral: Set[String]
  def removeNot: LogicWire
}
case class TWire(val s: String) extends LogicWire {
  def getLiteral = Set(s)
  def removeNot = TWire(s)
}
case class NWire(val s: String) extends LogicWire {
  def getLiteral = Set(s)
  def removeNot = TWire(s)
}
case class TWire1() extends LogicWire {
  val s = "static1"
  def getLiteral = Set(s)
  def removeNot = TWire1()
}
case class TWire0() extends LogicWire {
  val s = "static0"
  def getLiteral = Set(s)
  def removeNot = TWire0()
}

case class Disjunctive(val triple: Tuple3[LogicWire, LogicWire, LogicWire]) {
  def getInputs: Set[LogicWire] = Set(triple._1.removeNot, triple._2.removeNot, triple._3.removeNot)
  def contains(challenge: Disjunctive) = getInputs == challenge.getInputs
}
case class CNF(val ls: List[Disjunctive])

case class AND(val a1: LogicWire, val a2: LogicWire) extends LogicWire {
  val s: String = a1.s ++ "&&" ++ a2.s
  def getLiteral = a1.getLiteral ++ a2.getLiteral
  def removeNot = AND(a1.removeNot, a2.removeNot)
}

trait Tree[+A] {
  def add[S >: A](value: S): Tree[S]
  def map[B](f: A=>B): Tree[B]
  def toSet[S >: A](): Set[S]
  def foreach(f: A=>Unit):Unit
}
case class Node[+A] (val left: Tree[A], val right: Tree[A]) extends Tree[A] {
  def add[S >: A](v: S): Tree[S] = right match {
    case Leaf(None) => Node(left, Leaf(Some(v)))
    case Leaf(Some(x)) => Node(left, Node(Leaf(Some(x)), Leaf(Some(v))))
    case Node(l,r) => Node(left, Node(l,r.add(v)))
  }
  def map[B](f: A=>B) = Node(left.map(f), right.map(f))
  def toSet[S >: A]() = left.toSet() ++ right.toSet()
  def foreach(f: A=>Unit):Unit = {
    left.foreach(f)
    right.foreach(f)
  }
}
case class Leaf[A] (val value: Option[A]) extends Tree[A] {
  def add[S >: A](v: S): Tree[S] = value match {
    case None => Leaf(Some(v))
    case Some(_) => Node(this, Leaf(Some(v)))
  }
  def map[B](f: A=>B) = value match {
    case None => Leaf(None)
    case Some(v) => Leaf(Some(f(v)))
  }
  def toSet[S >: A](): Set[S] = value match {
    case None => Set()
    case Some(v) => Set(v)
  }
  def foreach(f: A => Unit):Unit = value match{
    case None => ()
    case Some(v) => f(v)
  }
}

object XOR2Oracle {
  import com.qu4s.core.{X, CX, CCX}
  def getAllLiterals(xor: Tree[LogicWire]): Set[String] = {
    xor.toSet().flatMap(_.getLiteral)
  }
  def xor2Circuit(xor: Tree[LogicWire])(implicit c: Circuit): Tuple2[Wire, Seq[Gate]] = {
    def helper(term: LogicWire)(implicit c: Circuit): Wire = term match {
      case TWire(s) => {
        val next_acc = c.newAccWire()
        CX(Wire(s), next_acc)
        next_acc
      }
      case TWire1() => {
        val next_acc = c.newAccWire()
        CX(Wire1(), next_acc)
        next_acc
      }
      case AND(a1, a2) => {
        a2 match {
          case TWire(s) => {
            val next_acc = c.newAccWire()
            CCX(Wire(a1.s), Wire(s), next_acc)
            next_acc
          }
          case AND(a3, a4) => {
            val next1_acc = c.newAccWire()
            val next2_acc = c.newAccWire()
            CCX(Wire(a3.s), Wire(a4.s), next1_acc)
            CCX(Wire(a1.s), next1_acc, next2_acc)
            next2_acc
          }
        }
      }
    }
    var acc: Option[Wire] = None
    xor.foreach(term => {
                  val fromTerm = helper(term)
                  acc match {
                    case None => {
                      acc = Some(fromTerm)
                    }
                    case Some(acc_v) => {
                      val output = c.newAccWire()
                      CX(acc_v, output)
                      CX(fromTerm, output)
                      acc = Some(output)
                    }
                  }

      })
    acc match{
      case None => (c.newAccWire(), c.gates)
      case Some(acc_v) => (acc_v, c.gates)
    }
  }
  def conjunctionXOR(accs: List[Wire], acc: Wire)(implicit c: Circuit): Wire = accs match {
    case Nil       => acc
    case hd :: Nil => acc
    case hd :: tl => {
      val next_acc = c.newAccWire()
      CCX(acc, hd, next_acc)
      conjunctionXOR(tl, next_acc)
    }
  }
  def translate(set: Set[Tree[LogicWire]]): Tuple2[Circuit, Wire] = {
    implicit val circuit = new Circuit
    val literals: Set[String] = set.flatMap(getAllLiterals(_))

    circuit.wires = literals.toSeq

    val conj_blocks = set.map(xor2Circuit(_)).toList
    val accs = conj_blocks.map(_._1)
    val xor_blocks = conj_blocks.map(_._2)
    val wo = conjunctionXOR(accs, circuit.newAccWire())
    import SATGateOps.ReverseGate
    for (ls <- xor_blocks; g <- ls){
      g.reapply()
    }
    (circuit, wo)
  }
}
object SATGateOps{
  implicit class ReverseGate(g: Gate)(implicit c: Circuit) {
    def reapply() = g match {
      case H(input) => H(input)
      case X(input) => X(input)
      case CX(a0, a1) => CX(a0, a1)
      case CCX(control1, control2, target) => CCX(control1, control2, target)
      case Cu1(control, target, theta) => Cu1(control, target, theta)
    }
  }
}
case class Conjunctive(val terms: List[Disjunctive])

object Conjunctive2XORTree {
  def eachXOR(ls: List[Disjunctive]): Tree[LogicWire] = {
    val disHead = ls.head
    val emptyTable = Disjunctive2XORTree.mkTruthTable(disHead)
    val resTable: Map[Tuple3[Boolean, Boolean, Boolean], Boolean] = ls.foldLeft(emptyTable) { (tb, dis) =>
      Disjunctive2XORTree.productOfTruthTable(tb, Disjunctive2XORTree.mkTruthTable(dis))
    }
    Disjunctive2XORTree.getXORTree(disHead)(resTable)
  }
  def get(c: Conjunctive): Set[Tree[LogicWire]] = {
    val grouped: Map[Set[LogicWire], List[Disjunctive]] = c.terms.groupBy(_.getInputs)
    grouped.values.map(eachXOR(_)).toSet
  }
}
object Disjunctive2XORTree {
  def mkTruthTable(d: Disjunctive) = {
    val helper: LogicWire => Boolean => Boolean = w => { a =>
      {
        w match {
          case TWire(_) => a
          case NWire(_) => !a
        }
      }
    }
    def f(w1: Boolean, w2: Boolean, w3: Boolean) =
      helper(d.triple._1)(w1) ||
        helper(d.triple._2)(w2) ||
        helper(d.triple._3)(w3)
    val tf = List(true, false)
    (for (a <- tf; b <- tf; c <- tf) yield ((a, b, c) -> f(a, b, c))).toMap
  }
  def productOfTruthTable(
      a1: Map[Tuple3[Boolean, Boolean, Boolean], Boolean],
      a2: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]
  ): Map[Tuple3[Boolean, Boolean, Boolean], Boolean] = {
    val tf = List(true, false)

    (for (a <- tf; b <- tf; c <- tf)
      yield (
        (a, b, c) -> (a1(a, b, c) && a2(a, b, c))
      )).toMap
  }
  def isClauseAllFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) = if (tt(false, false, false)) {
    true
  } else {
    false
  }
  def isClause2False(
      tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]
  )(a1: Tuple3[Boolean, Boolean, Boolean], a2: Tuple3[Boolean, Boolean, Boolean]) =
    List(tt(a1), tt(a2)).count(a => a == true) % 2 == 1

  def isClauseABFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause2False(tt)((false, false, true), (false, false, false))
  def isClauseBCFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause2False(tt)((true, false, false), (false, false, false))
  def isClauseCAFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause2False(tt)((false, true, false), (false, false, false))
  def isClause1False(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean])(
      a1: Tuple3[Boolean, Boolean, Boolean],
      a2: Tuple3[Boolean, Boolean, Boolean],
      a3: Tuple3[Boolean, Boolean, Boolean],
      a4: Tuple3[Boolean, Boolean, Boolean]
  ) =
    List(tt(a1), tt(a2), tt(a3), tt(a4)).count(a => a == true) % 2 == 1

  def isClauseAFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause1False(tt)(
      (false, true, true),
      (false, true, false),
      (false, false, true),
      (false, false, false)
    )
  def isClauseBFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause1False(tt)(
      (true, false, true),
      (true, false, false),
      (false, false, true),
      (false, false, false)
    )
  def isClauseCFalse(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    isClause1False(tt)(
      (true, true, false),
      (true, false, false),
      (false, true, false),
      (false, false, false)
    )
  def isClauseAll(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]) =
    tt.values.count(a => a == true) % 2 == 1

  def getXORTree(d: Disjunctive)(tt: Map[Tuple3[Boolean, Boolean, Boolean], Boolean]): Tree[LogicWire] = {

    var clauses: Tree[LogicWire] = Leaf(None)

    clauses = if (isClauseAllFalse(tt)) {
      clauses.add(TWire1())
    } else clauses

    clauses = if (isClauseABFalse(tt)) {
      clauses.add(d.triple._3.removeNot)
    } else clauses

    clauses = if (isClauseBCFalse(tt)) {
      clauses.add(d.triple._1.removeNot)
    } else clauses

    clauses = if (isClauseCAFalse(tt)) {
      clauses.add(d.triple._2.removeNot)
    } else clauses

    clauses = if (isClauseAFalse(tt)) {
      clauses.add(AND(d.triple._2, d.triple._3).removeNot)
    } else clauses

    clauses = if (isClauseBFalse(tt)) {
      clauses.add(AND(d.triple._1, d.triple._3).removeNot)
    } else clauses

    clauses = if (isClauseCFalse(tt)) {
      clauses.add(AND(d.triple._1, d.triple._2).removeNot)
    } else clauses

    clauses = if (isClauseAll(tt)) {
      clauses.add(AND(d.triple._1, AND(d.triple._2, d.triple._3)).removeNot)
    } else clauses

    clauses
  }
}

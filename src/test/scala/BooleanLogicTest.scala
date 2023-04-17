package com.qu4s.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.qu4s.BooleanLogic._

class BooleanLogicTestSpec extends AnyFlatSpec with Matchers {
  val tA = TWire("A")
  val tB = TWire("B")
  val tC = TWire("C")
  val tD = TWire("D")
  val nA = NWire("A")
  val nB = NWire("B")
  val nC = NWire("C")
  val nD = NWire("D")

  val d = Disjunctive(tA, tB, tC)
  val tt = Map(
    (true, true, true) -> true,
    (false, true, true) -> true,
    (true, false, true) -> true,
    (false, false, true) -> true,
    (true, true, false) -> true,
    (false, true, false) -> true,
    (true, false, false) -> true,
    (false, false, false) -> false
  )
  val d2 = Disjunctive(tA, tB, nC)
  val tt2 = Map(
    (true, true, true) -> true,
    (false, true, true) -> true,
    (true, false, true) -> true,
    (false, false, true) -> false,
    (true, true, false) -> true,
    (false, true, false) -> true,
    (true, false, false) -> true,
    (false, false, false) -> true
  )
 "A ∨ B ∨ C" should "give right truth table" in {
    Disjunctive2XORTree.mkTruthTable(d) shouldEqual tt
  }
  "A ∨ B ∨ C" should "count 0 if all false" in {
    Disjunctive2XORTree.isClauseAllFalse(tt) shouldEqual false
  }
  "A ∨ B ∨ C" should "matches XOR clause" in {
    Disjunctive2XORTree.getXORTree(d)(tt).toSet() shouldEqual (
      Node(
        Leaf(Some(AND(tA, AND(tB, tC)))),
        Node(
          Leaf(Some(AND(tA, tB))),
          Node(
            Leaf( Some(AND(tB, tC)) ),
            Node(
              Leaf( Some(AND(tA, tC)) ),
              Node(
                Leaf( Some(tA) ),
                Node(
                  Leaf(Some(tB)),
                  Leaf(Some(tC))
                )
              )
            )
          )
        )
      ).toSet()
    )
  }
  "A ∨ B ∨ ~C" should "contains A∨B∨C" in {
    d2.contains(d) shouldBe true
  }
  "A ∨ B ∨ ~C" should "not contains A∨B∨D" in {
    val d3 = Disjunctive(Tuple3(TWire("A"), TWire("B"), TWire("D")))
    d2.contains(d3) shouldBe false
  }

  "A ∨ B ∨ ~C" should "give right truth table" in {
    Disjunctive2XORTree.mkTruthTable(d2) shouldEqual tt2
  }
  "A ∨ B ∨ ~C" should "count 0 if all false" in {
    Disjunctive2XORTree.isClauseAllFalse(tt2) shouldEqual true
  }
  "A ∨ B ∨ ~C" should "matches XOR clause" in {
    Disjunctive2XORTree.getXORTree(d2)(tt2).toSet() shouldEqual Node(
      Leaf(Some(TWire1())),
      Node(
        Leaf(Some(AND(tA, AND(tB, tC)))),
        Node(
          Leaf(Some(AND(tB, tC))),
          Node(
            Leaf(Some(AND(tA, tC))),
            Leaf(Some(tC))
          )
        )
      )
    ).toSet()
  }
  "(A ∨ B ∨ C)∧(A ∨ B ∨ ¬C)" should "matches XOR clause A⊕B⊕AB based on truth table" in {
    val tt3 = Disjunctive2XORTree.productOfTruthTable(tt, tt2)
    Disjunctive2XORTree.getXORTree(d)(tt3).toSet() shouldEqual Node(
      Leaf(Some(AND(tA, tB))),
      Node(
        Leaf(Some(tA)),
        Leaf(Some(tB))
      )
    ).toSet()
  }
  "(A ∨ B ∨ C)∧(A ∨ B ∨ ¬C)" should "matches XOR clause A⊕B⊕AB" in {
    val con = Conjunctive(List(d, d2))
    Conjunctive2XORTree.get(con).head.toSet() shouldEqual Node(
      Leaf(Some(AND(tA, tB))),
      Node(
        Leaf(Some(tA)),
        Leaf(Some(tB))
      )
    ).toSet()
  }
  "(A∨B∨C)∧(A∨¬B∨¬C)∧(¬A∨B∨¬C)∧(¬A∨¬B∨C)" should "matches XOR clause A⊕B⊕C" in {
    val con = Conjunctive(
      List(
        Disjunctive(tA, tB, tC),
        Disjunctive(tA, nB, nC),
        Disjunctive(nA, tB, nC),
        Disjunctive(nA, nB, tC)
      )
    )
    Conjunctive2XORTree.get(con).head.toSet() shouldEqual Node(
      Leaf(Some(tA)),
      Node(
        Leaf(Some(tB)),
        Leaf(Some(tC))
      )
    ).toSet()
  }
  val abd = Node(
    Leaf( Some( AND(tB, tD) ) ),
    Node(
      Leaf(Some(tD)),
      Node(
        Leaf(Some(tB)),
        Node(
          Leaf(Some(AND(tA, AND(tB, tD)))),
          Node(
            Leaf(Some(AND(tA, tB))),
            Node(
              Leaf(Some( AND(tA, tD) )),
              Leaf(Some(tA))
            )
          )
        )
      )
    )
  )
  val dis_abd = Disjunctive(tA, tB, tD)
  "(A∨B∨D)" should "matches XOR clause (A∧B∧D)⊕(A∧B)⊕(A∧D)⊕(B∧D)⊕A⊕B⊕D" in {
    val con = Conjunctive(dis_abd :: Nil)
    Conjunctive2XORTree.get(con).head.toSet() shouldEqual abd.toSet()
  }
  val abcs = Node(
    Leaf(Some(AND(tA, tB))),
    Node(
      Leaf(Some(AND(tB, tC))),
      Node(
        Leaf(Some(AND(tA, AND(tB, tC)))),
        Node(
          Leaf(Some(TWire1())),
          Leaf(Some(AND(tA, tC)))
        )
      )
    )
  )
  val dis_list_abcs = List(
    Disjunctive(tA, nB, nC),
    Disjunctive(nA, tB, nC),
    Disjunctive(nA, nB, tC)
  )
  "(A∨¬B∨¬C)∧(¬A∨B∨¬C)∧(¬A∨¬B∨C)" should "(a∧b∧c)⊕(a∧b)⊕(b∧c)⊕(c∧a)⊕true" in {
    val con = Conjunctive(dis_list_abcs)
    Conjunctive2XORTree.get(con).head.toSet() shouldEqual abcs.toSet()
  }
  "(A∨B∨D)∧(A∨¬B∨¬C)∧(¬A∨B∨¬C)∧(¬A∨¬B∨C)" should "matches XOR clause ((A∧B∧D)⊕(A∧B)⊕(A∧D)⊕(B∧D)⊕A⊕B⊕D)∧( (A∧B∧C)⊕(A∧B)⊕(B∧C)⊕(C∧A)⊕True )" in {
    val con = Conjunctive(dis_abd :: dis_list_abcs)
    Conjunctive2XORTree.get(con).map(_.toSet()) shouldEqual Set(
      abcs.toSet(),
      abd.toSet()
    )
  }
  import com.qu4s.core._
  def make_AB_A_B(): Circuit = {
    implicit val challenge_AB_A_B = new Circuit()
    val wA = Wire("A")
    val wB = Wire("B")
    val acc0 = Wire("acc0")
    val acc1 = Wire("acc1")
    val acc2 = Wire("acc2")
    val acc3 = Wire("acc3")
    val acc4 = Wire("acc4")
    CX(wA,acc0)
    CX(wB,acc1)
    CX(acc0,acc2)
    CX(acc1,acc2)
    CCX(wA,wB,acc3)
    CX(acc2,acc4)
    CX(acc3,acc4)

    CX(wA,acc0)
    CX(wB,acc1)
    CX(acc0,acc2)
    CX(acc1,acc2)
    CCX(wA,wB,acc3)
    CX(acc2,acc4)
    CX(acc3,acc4)

    challenge_AB_A_B
  }
  "XOR clause AB⊕A⊕B" should "generate an oracle circuit" in {
    val circuit = XOR2Oracle
      .translate(
        Set(
          Node(
            Leaf(Some(tA)),
            Node(
              Leaf(Some(tB)),
              Leaf(Some(AND(tA, tB)))
            )
          )
        )
      )
      ._1
    circuit.gates shouldEqual make_AB_A_B().gates
  }
  "(A∨B∨C)∧(A∨B∨¬C) aka AB⊕A⊕B" should "generate an oracle circuit" in {
    val con = Conjunctive(List(d, d2))
    val circuit = XOR2Oracle.translate(Conjunctive2XORTree.get(con))._1
    circuit.gates shouldEqual make_AB_A_B().gates
  }
  "(A∨¬B∨¬C)∧(¬A∨B∨¬C)∧(¬A∨¬B∨C) aka (a∧b∧c)⊕(a∧b)⊕(b∧c)⊕(c∧a)⊕true" should "generate an oracle circuit"in {
    val con = Conjunctive(dis_list_abcs)
    val circuit = XOR2Oracle.translate(Conjunctive2XORTree.get(con))._1

    implicit val challenge_ABC = new Circuit()

    val wA = Wire("A")
    val wB = Wire("B")
    val wC = Wire("C")
    val acc0 = Wire("acc0")
    val acc1 = Wire("acc1")
    val acc2 = Wire("acc2")
    val acc3 = Wire("acc3")
    val acc4 = Wire("acc4")
    val acc5 = Wire("acc5")
    val acc6 = Wire("acc6")
    val acc7 = Wire("acc7")
    val acc8 = Wire("acc8")
    val acc9 = Wire("acc9")


    CX(Wire1(),acc0)
    CCX(wB,wC,acc1)
    CX(acc0,acc2)
    CX(acc1,acc2)
    CCX(wA,wC,acc3)
    CX(acc2,acc4)
    CX(acc3,acc4)
    CCX(wA,wB,acc5)
    CX(acc4,acc6)
    CX(acc5,acc6)
    CCX(wB,wC,acc7)
    CCX(wA,acc7,acc8)
    CX(acc6,acc9)
    CX(acc8,acc9)

    CX(Wire1(),acc0)
    CCX(wB,wC,acc1)
    CX(acc0,acc2)
    CX(acc1,acc2)
    CCX(wA,wC,acc3)
    CX(acc2,acc4)
    CX(acc3,acc4)
    CCX(wA,wB,acc5)
    CX(acc4,acc6)
    CX(acc5,acc6)
    CCX(wB,wC,acc7)
    CCX(wA,acc7,acc8)
    CX(acc6,acc9)
    CX(acc8,acc9)


    println(circuit.gates)
    circuit.gates shouldEqual challenge_ABC.gates
  }
}

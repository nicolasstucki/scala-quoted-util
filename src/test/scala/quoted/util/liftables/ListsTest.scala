package scala.quoted
package util
package liftables

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._
import scala.quoted.util.Lists._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class ListsTest {

  @Test def testLifing: Unit = {
    val nil: Expr[List[Int]] = Nil
    val l1: Expr[List[Int]] = List(1)
    val l2: Expr[List[Int]] = List(1, 2)

    assertEquals(Nil, nil.run)
    assertEquals(List(1), l1.run)
    assertEquals(List(1, 2), l2.run)

    assertEquals(0, nil.foldLeft(0)('{ (acc, x) => acc + x }).run)
    assertEquals(1, l1.foldLeft(0)('{ (acc, x) => acc + x }).run)
    assertEquals(3, l2.foldLeft(0)('{ (acc, x) => acc + x }).run)


    assertEquals(0, (Nil: List[Int]).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)
    assertEquals("0", (Nil: List[Int]).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)

    assertEquals(1, List(1).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)
    assertEquals(
      """{
        |  0.+(1)
        |}""".stripMargin,
        List(1).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)
    assertEquals(3, List(1, 2).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)

    assertEquals( // TODO improve printer
      """{
        |  {
        |    0.+(1)
        |  }.+(2)
        |}""".stripMargin,
        List(1, 2).unrolledFoldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)
  }

}
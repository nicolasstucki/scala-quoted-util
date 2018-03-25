package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.UnrolledExpr._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Toolbox._

class ListsTest {

  @Test def listOps: Unit = {
    val nil: Expr[List[Int]] = Nil.toExpr
    val l1: Expr[List[Int]] = List(1).toExpr
    val l2: Expr[List[Int]] = List(1, 2).toExpr

    assertEquals(0, (Nil: List[Int]).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)
    assertEquals("0", (Nil: List[Int]).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)

    assertEquals(1, List(1).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)
    assertEquals(
      """0.+(1)""".stripMargin,
        List(1).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)
    assertEquals(3, List(1, 2).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).run)

    assertEquals( // TODO improve printer
      """0.+(1).+(2)""".stripMargin,
        List(1, 2).unrolled.foldLeft(0.toExpr)((acc, x) => '{ ~acc + ~x.toExpr } ).show)
  }

}
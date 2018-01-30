package scala.quoted
package util

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class ListsTest {

  @Test def listOps: Unit = {
    val nil: Expr[List[Int]] = Nil
    val l1: Expr[List[Int]] = List(1)
    val l2: Expr[List[Int]] = List(1, 2)

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
package scala.quoted
package util

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class BlocksTest {

  @Test def blocks: Unit = {
    for (i <- 0 to 10) {
      def stats1(sb: Expr[StringBuilder]): List[Expr[_]] = List.fill(i)('{ (~sb).append("a") })
      val blockExpr = '{ val sb = new StringBuilder; ~stats1('(sb)).toBlock('(sb.result)) }
      assertEquals("a" * i, blockExpr.run)
    }
  }

  @Test def statements: Unit = {
    for (i <- 0 to 10) {
      def stats1(sb: Expr[StringBuilder]): List[Expr[_]] = List.fill(i)('{ (~sb).append("a") })
      val statsExpr = '{ val sb = new StringBuilder; ~stats1('(sb)).toStatements; sb.result }
      assertEquals("a" * i, statsExpr.run)
    }
  }

}
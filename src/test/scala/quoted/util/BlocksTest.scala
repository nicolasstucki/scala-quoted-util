package scala.quoted
package util

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._
import scala.quoted.util.Blocks._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class BlocksTest {

  @Test def listOps: Unit = {
    for (i <- 0 to 10) {
      def stats1(sb: Expr[StringBuilder]): List[Expr[_]] = List.fill(i)('{ (~sb).append("a") })
      val blockExpr = '{ val sb = new StringBuilder; ~block(stats1('(sb)), '(sb.result)) }
      val statsExpr = '{ val sb = new StringBuilder; ~stats(stats1('(sb))); sb.result }
      assertEquals("a" * i, blockExpr.run)
      assertEquals("a" * i, statsExpr.run)
    }
  }

}
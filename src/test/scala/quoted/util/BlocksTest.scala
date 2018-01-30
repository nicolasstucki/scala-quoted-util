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
      def stats(sb: Expr[StringBuilder]): List[Expr[_]] = List.fill(i)('{ (~sb).append("a") })
      val expr = '{ val sb = new StringBuilder; ~block(stats('(sb)), '(sb.result)) }
      assertEquals("a" * i, expr.run)
    }
  }

}
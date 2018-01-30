package scala.quoted
package util
package liftables

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._
import scala.quoted.util.Lists._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class OptionsTest {

  @Test def testLifing: Unit = {
    val none: Expr[Option[Int]] = None
    val some: Expr[Option[Int]] = Some(1)

    assertEquals(None, none.run)
    assertEquals(Some(1), some.run)
  }

}
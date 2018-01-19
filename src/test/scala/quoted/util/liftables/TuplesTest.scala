package scala.quoted
package util
package liftables

import org.junit.Test
import org.junit.Assert._

import util.Runners._

class TuplesTest {

  @Test def testLifing: Unit = {
    // FixClasspath()
    import Tuples._
    val t1: Expr[Tuple1[Int]] = Tuple1(1)
    val t2: Expr[(Int, Int)] = (1, 2)
    val t3: Expr[(Int, Int, Int)] = (1, 2, 3)
    val t4: Expr[(Int, Int, Int, Int)] = (1, 2, 3, 4)

    assertEquals(Tuple1(1), t1.run)
    assertEquals((1, 2), t2.run)
    assertEquals((1, 2, 3), t3.run)
    assertEquals((1, 2, 3, 4), t4.run)
  }

}
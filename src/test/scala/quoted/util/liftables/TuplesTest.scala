package scala.quoted
package util
package liftables

import org.junit.Test
import org.junit.Assert._

import util.Runners._

class TuplesTest {

  @Test def testLifing: Unit = {
    import Tuples._
    val t1: Expr[Tuple1[Int]] = Tuple1(1)
    val t2: Expr[(Int, Int)] = (1, 2)
    val t3: Expr[(Int, Int, Int)] = (1, 2, 3)
    val t4: Expr[(Int, Int, Int, Int)] = (1, 2, 3, 4)
    val t5: Expr[(Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5)
    val t6: Expr[(Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6)
    val t7: Expr[(Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7)
    val t8: Expr[(Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8)
    val t9: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9)
    val t10: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val t11: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val t12: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val t13: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    val t14: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    val t15: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    val t16: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val t17: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    val t18: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
    val t19: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
    val t20: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    val t21: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val t22: Expr[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,17, 18, 19, 20, 21, 22)

    assertEquals(Tuple1(1), t1.run)
    assertEquals((1, 2), t2.run)
    assertEquals((1, 2, 3), t3.run)
    assertEquals((1, 2, 3, 4), t4.run)
    assertEquals((1, 2, 3, 4, 5), t5.run)
    assertEquals((1, 2, 3, 4, 5, 6), t6.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7), t7.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8), t8.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9), t9.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10), t10.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), t11.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), t12.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), t13.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), t14.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), t15.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), t16.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17), t17.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), t18.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), t19.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), t20.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), t21.run)
    assertEquals((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), t22.run)
  }

}
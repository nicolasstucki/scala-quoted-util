package scala.quoted
package util

import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._

import org.junit.Test
import org.junit.Assert._

import dotty.tools.dotc.quoted.Runners._

class LiftTest {

  @Test def unit: Unit = {
    val unit: Expr[Unit] = ()
    assertEquals((), unit.run)
  }

  @Test def options: Unit = {
    val none: Expr[Option[Int]] = None
    val some: Expr[Option[Int]] = Some(1)

    assertEquals(None, none.run)
    assertEquals(Some(1), some.run)
  }

  @Test def lists: Unit = {
    val nil: Expr[List[Int]] = Nil
    val l1: Expr[List[Int]] = List(1)
    val l2: Expr[List[Int]] = List(1, 2)

    assertEquals(Nil, nil.run)
    assertEquals(List(1), l1.run)
    assertEquals(List(1, 2), l2.run)
  }

  @Test def byteArrays: Unit = {
    val nil: Expr[Array[Byte]] = Array.empty[Byte]
    val a1: Expr[Array[Byte]] = Array[Byte](1)
    val a2: Expr[Array[Byte]] = Array[Byte](1, 2)
    assertArrayEquals(Array.empty[Byte], nil.run)
    assertArrayEquals(Array[Byte](1), a1.run)
    assertArrayEquals(Array[Byte](1, 2), a2.run)
  }

  @Test def shortArrays: Unit = {
    val nil: Expr[Array[Short]] = Array.empty[Short]
    val a1: Expr[Array[Short]] = Array[Short](1)
    val a2: Expr[Array[Short]] = Array[Short](1, 2)
    assertArrayEquals(Array.empty[Short], nil.run)
    assertArrayEquals(Array[Short](1), a1.run)
    assertArrayEquals(Array[Short](1, 2), a2.run)
  }

  @Test def charArrays: Unit = {
    val nil: Expr[Array[Char]] = Array.empty[Char]
    val a1: Expr[Array[Char]] = Array[Char]('a')
    val a2: Expr[Array[Char]] = Array[Char]('a', 'b')
    assertArrayEquals(Array.empty[Char], nil.run)
    assertArrayEquals(Array[Char]('a'), a1.run)
    assertArrayEquals(Array[Char]('a', 'b'), a2.run)
  }

  @Test def intArrays: Unit = {
    val nil: Expr[Array[Int]] = Array.empty[Int]
    val a1: Expr[Array[Int]] = Array(1)
    val a2: Expr[Array[Int]] = Array(1, 2)
    assertArrayEquals(Array.empty[Int], nil.run)
    assertArrayEquals(Array(1), a1.run)
    assertArrayEquals(Array(1, 2), a2.run)
  }

  @Test def longArrays: Unit = {
    val nil: Expr[Array[Long]] = Array.empty[Long]
    val a1: Expr[Array[Long]] = Array(1L)
    val a2: Expr[Array[Long]] = Array(1L, 2L)
    assertArrayEquals(Array.empty[Long], nil.run)
    assertArrayEquals(Array(1L), a1.run)
    assertArrayEquals(Array(1L, 2L), a2.run)
  }

  @Test def floatArrays: Unit = {
    val nil: Expr[Array[Float]] = Array.empty[Float]
    val a1: Expr[Array[Float]] = Array(1.0f)
    val a2: Expr[Array[Float]] = Array(1.0f, 2.0f)
    assertArrayEquals(Array.empty[Float], nil.run, 0.0f)
    assertArrayEquals(Array(1.0f), a1.run, 0.0f)
    assertArrayEquals(Array(1.0f, 2.0f), a2.run, 0.0f)
  }

  @Test def doubleArrays: Unit = {
    val nil: Expr[Array[Double]] = Array.empty[Double]
    val a1: Expr[Array[Double]] = Array[Double](1.0)
    val a2: Expr[Array[Double]] = Array[Double](1.0, 2.0)
    assertArrayEquals(Array.empty[Double], nil.run, 0.0)
    assertArrayEquals(Array[Double](1.0), a1.run, 0.0)
    assertArrayEquals(Array[Double](1.0, 2.0), a2.run, 0.0)
  }

  @Test def arrays: Unit = {
    // TODO find a way to lift ClassTags
    implicit val ct: Expr[reflect.ClassTag[String]] = '{implicitly[reflect.ClassTag[String]]}

    val nil: Expr[Array[String]] = Array.empty[String]
    val a1: Expr[Array[String]] = Array("a")
    val a2: Expr[Array[String]] = Array("a", "b")

    assertArrayEquals(Array.empty[Object], nil.run.asInstanceOf[Array[Object]])
    assertArrayEquals(Array[Object]("a"), a1.run.asInstanceOf[Array[Object]])
    assertArrayEquals(Array[Object]("a", "b"), a2.run.asInstanceOf[Array[Object]])
  }

  @Test def tuples: Unit = {
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
import scala.quoted._

import scala.quoted.util.Let._
import scala.quoted.util.Lifters._
import scala.quoted.util.UnrolledExpr._

import dotty.tools.dotc.quoted.Toolbox._

import org.junit.Test

object ShonanChallenge {

  implicit val ct: Expr[Class[Array[Int]]] = '(classOf[Array[Int]])

  val array = Array(
    Array(1, 1, 1, 1, 1), // dense
    Array(0, 0, 0, 0, 0), // null
    Array(0, 0, 1, 0, 0), // sparse
    Array(0, 0, 0, 0, 0),
    Array(0, 0, 1, 0, 1)
  )

  def main(args: Array[String]): Unit = {
    val v = Array(1,1,1,1,1)

    val v1 = matrix_vector_prod(array, v)
    println(v1.mkString("Array(", ", ", ")"))
    println()
    println()

    val v2 = static(array) {
      array => '{
        val va = ~v.toExpr
        ~matrix_vector_prod_staged(array, '(va))
      }
    }
    println(v2.show)
    println(v2.run.mkString("Array(", ", ", ")"))

    val v3 = static(array) {
      array => '{
        val va = ~v.toExpr
        ~matrix_vector_prod_staged_unrooled(array, '(va))
      }
    }
    println(v3.show)
    println(v3.run.mkString("Array(", ", ", ")"))

    val v4 = static(array) {
      array => '{
        val va = ~v.toExpr
        ~matrix_vector_prod_staged_unrooled_folded(array, '(va))
      }
    }
    println(v4.show)
    println(v4.run.mkString("Array(", ", ", ")"))
  }

  def matrix_vector_prod(a: Array[Array[Int]], v: Array[Int]): Array[Int] = {
    val n = a.length
    val v1 = new Array[Int](n)
    for (i <- (0 until n)) {
      for (j <- (0 until n))
        v1(i) = v1(i) + a(i)(j) * v(j)
    }
    v1
  }

  def matrix_vector_prod_staged(a: Static[Array[Array[Int]]], v: Expr[Array[Int]]): Expr[Array[Int]] = '{
    val n = ~a.value.length.toExpr
    val v1 = new Array[Int](n)
    for (i <- (0 until n)) {
      for (j <- (0 until n))
        v1(i) = v1(i) + (~a.ref)(i)(j) * (~v)(j)
    }
    v1
  }

  def matrix_vector_prod_staged_unrooled(a: Static[Array[Array[Int]]], v: Expr[Array[Int]]): Expr[Array[Int]] = {
    val n = a.value.length
    def conditionalyUnrolledInnerLoop(v1: Expr[Array[Int]], i: Int) = {
      val row = a.value(i)
      val sparse = row.count(_ != 0) < 3
      if (sparse) { // unrolled loop
        for (j <- (0 until n).unrolled; if row(j) != 0)
          '((~v1)(~i.toExpr) = (~v1)(~i.toExpr) + ~a.value(i)(j).toExpr * (~v)(~j.toExpr))
      } else '{ // quoted loop
        val row = (~a.ref)(~i.toExpr)
        for (j <- (0 until ~n.toExpr))
          (~v1)(~i.toExpr) = (~v1)(~i.toExpr) + row(j) * (~v)(j)
      }
    }
    '{
      val v1 = new Array[Int](~n.toExpr)
      ~{ for (i <- (0 until n).unrolled) conditionalyUnrolledInnerLoop('(v1), i) } // Unroll outer loop
      v1
    }
  }


  def matrix_vector_prod_staged_unrooled_folded(a: Static[Array[Array[Int]]], v: Expr[Array[Int]]): Expr[Array[Int]] = {
    val n = a.value.length
    def conditionalyUnrolledInnerLoop(v1: Expr[Array[Int]], i: Int) = {
      val row = a.value(i)
      val sparse = row.count(_ != 0) < 3
      if (sparse) { // unrolled loop
        def rhs = row.zipWithIndex.foldLeft('(0)) { case (acc, (value, j)) =>
          if (value == 0) acc
          else '{ ~acc + ~value.toExpr * (~v)(~j.toExpr) }
        }
        if (row.forall(_ == 0)) '()
        else '{ (~v1)(~i.toExpr) = ~rhs }
      } else '{ // quoted loop
        val row = (~a.ref)(~i.toExpr)
        for (j <- (0 until ~n.toExpr))
          (~v1)(~i.toExpr) = (~v1)(~i.toExpr) + row(j) * (~v)(j)
      }
    }
    '{
      val v1 = new Array[Int](~n.toExpr)
      ~{ for (i <- (0 until n).unrolled) conditionalyUnrolledInnerLoop('(v1), i) } // Unroll outer loop
      v1
    }
  }

}

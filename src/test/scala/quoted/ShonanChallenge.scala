import scala.quoted._

import scala.quoted.util.Lifters._
import scala.quoted.util.Unrolled._

import dotty.tools.dotc.quoted.Toolbox._

object ShonanChallenge {

  implicit val ct: Expr[reflect.ClassTag[Array[Int]]] = '(reflect.ClassTag(classOf[Array[Int]]))

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

    val v2 = withStatic(array) {
      array => '{
        val va = ~v.toExpr
        ~matrix_vector_prod_staged(array, '(va))
      }
    }
    println(v2.show)
    println(v2.run.mkString("Array(", ", ", ")"))

    val v3 = withStatic(array) {
      array => '{
        val va = ~v.toExpr
        ~matrix_vector_prod_staged_unrooled(array, '(va))
      }
    }
    println(v3.show)
    println(v3.run.mkString("Array(", ", ", ")"))
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
    '{
      val v1 = new Array[Int](~n.toExpr)
      ~{
        for (i <- (0 until n).unrolled) {
          val row = a.value(i)
          val sparse = row.count(_ != 0) < 3
          if (sparse) {
            for (j <- (0 until n).unrolled; if row(j) != 0)
              '(v1(~i.toExpr) = v1(~i.toExpr) + (~a.ref)(~i.toExpr)(~j.toExpr) * (~v)(~j.toExpr))
          } else '{
            for (j <- (0 until ~n.toExpr))
              v1(~i.toExpr) = v1(~i.toExpr) + (~a.ref)(~i.toExpr)(j) * (~v)(j)
          }
        }
      }
      v1
    }
  }


  implicit class Unrolled[T: Liftable](xs: Seq[T]) {
    def unrolled: UnrolledOps[T] = new UnrolledOps(xs)
  }
  class UnrolledOps[T: Liftable](xs: Seq[T]) {
    def foreach[U](f: T => Expr[U]): Expr[Unit] = {
      xs.map(f).toStatements
    }

    def withFilter(f: T => Boolean): UnrolledOps[T] = new UnrolledOps(xs.filter(f))
  }


  final class Static[T](val value: T, val ref: Expr[T])

  def withStatic[T : Liftable : Type, U](x: T)(body: Static[T] => Expr[U]): Expr[U] = '{
    val static_x = ~x.toExpr
    ~body(new Static(x, '(static_x)))
  }
}

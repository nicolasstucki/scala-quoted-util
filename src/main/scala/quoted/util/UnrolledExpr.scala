package scala.quoted
package util

import scala.quoted.util.Lifters._

object UnrolledExpr {

  implicit class Unrolled[T: Liftable](xs: Seq[T]) {
    def unrolled: UnrolledExpr[T] = new UnrolledExpr(xs)
  }

  // implicit class BlockOps(seq: Seq[Expr[_]]) {
  //   /** Lifts a sequence of expressions Seq(x1, x2, ..., xn) to an expression '{ ~x1; ~x2; ...; ~xn; () } */
  //   def toStatements: Expr[Unit] = block(seq, '())
  //   /** Lifts a sequence of expressions Seq(x1, x2, ..., xn) and expression expr to an expression '{ ~x1; ~x2; ...; ~xn; ~expr } */
  //   def toBlock[T](expr: Expr[T]): Expr[T] = block(seq, expr)
  // }

  // TODO support blocks in the compiler to avoid creating trees of blocks?
  def block[T](stats: Seq[Expr[_]], expr: Expr[T]): Expr[T] = stats match {
    case Seq() => expr
    case Seq(x1) => '{ ~x1; ~expr }
    case Seq(x1, x2) => '{ ~x1; ~x2; ~expr }
    case Seq(x1, x2, x3) => '{ ~x1; ~x2; ~x3; ~expr }
    case Seq(x1, x2, x3, x4) => '{ ~x1; ~x2; ~x3; ~x4; ~expr }
    case _ => // TODO make tree shallower
      val (stats1, stats2) = stats.splitAt(stats.length / 2)
      '{ ~block(stats1, '()); ~block(stats2, expr) }
  }

}

class UnrolledExpr[T: Liftable](xs: Seq[T]) {
  import UnrolledExpr._

  def foreach[U](f: T => Expr[U]): Expr[Unit] = block(xs.map(f), '())

  def withFilter(f: T => Boolean): UnrolledExpr[T] = new UnrolledExpr(xs.filter(f))

  def foldLeft[U](acc: Expr[U])(f: (Expr[U], T) => Expr[U]): Expr[U] =
    xs.foldLeft(acc)((acc, x) => f(acc, x))
}

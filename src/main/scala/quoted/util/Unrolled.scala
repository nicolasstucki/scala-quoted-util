package scala.quoted
package util

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._

object Unrolled {

  implicit class UnrolledSeqOps[T : Liftable](seq: Seq[T]) {
    def unrolledFoldLeft[U](acc: Expr[U])(f: (Expr[U], T) => Expr[U]): Expr[U] =
      seq.foldLeft(acc)((acc, x) => f(acc, x))
  }

  implicit class BlockOps(seq: Seq[Expr[_]]) {
    /** Lifts a sequence of expressions Seq(x1, x2, ..., xn) to an expression '{ ~x1; ~x2; ...; ~xn; () } */
    def toStatements: Expr[Unit] = block(seq, ())
    /** Lifts a sequence of expressions Seq(x1, x2, ..., xn) and expression expr to an expression '{ ~x1; ~x2; ...; ~xn; ~expr } */
    def toBlock[T](expr: Expr[T]): Expr[T] = block(seq, expr)
  }

  // TODO support blocks in the compiler to avoid creating trees of blocks?
  private def block[T](stats: Seq[Expr[_]], expr: Expr[T]): Expr[T] = stats match {
    case Seq() => expr
    case Seq(x1) => '{ ~x1; ~expr }
    case Seq(x1, x2) => '{ ~x1; ~x2; ~expr }
    case Seq(x1, x2, x3) => '{ ~x1; ~x2; ~x3; ~expr }
    case Seq(x1, x2, x3, x4) => '{ ~x1; ~x2; ~x3; ~x4; ~expr }
    case _ => // TODO make tree shallower
      val (stats1, stats2) = stats.splitAt(stats.length / 2)
      '{ ~block(stats1, ()); ~block(stats2, expr) }
  }
}
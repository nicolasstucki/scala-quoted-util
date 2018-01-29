package scala.quoted
package util.liftables

import scala.quoted.Liftable._

object Lists {
  implicit def ListIsLiftable[T : Liftable](implicit t: Type[T]): Liftable[List[T]] = {
    case x :: xs  => '{ ~x.toExpr :: ~xs.toExpr }
    case Nil => '{ Nil: List[~t] }
  }

  implicit class LiftedOps[T : Liftable](list: Expr[List[T]]) {
    def foldLeft[U : Type](acc: Expr[U])(f: Expr[(U, T) => U]): Expr[U] =
      '{ (~list).foldLeft(~acc)(~f) }
  }

  implicit class UnrolledOps[T : Liftable](list: List[T]) {
    def unrolledFoldLeft[U](acc: Expr[U])(f: (Expr[U], T) => Expr[U]): Expr[U] = list match {
      case x :: xs => xs.unrolledFoldLeft(f(acc, x))(f)
      case Nil => acc
    }
  }
}
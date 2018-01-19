package scala.quoted
package util.liftables

import AnyToExpr._

object Lists {
  implicit def ListIsLiftable[T: Liftable](implicit t: Type[T]): Liftable[List[T]] = {
    case x :: xs  => '{ (~xs.toExpr).::[~t](~x.toExpr) }
    case Nil => '{ Nil: List[~t] }
  }

  implicit class LiftedOps[T: Liftable](list: Expr[List[T]]) {
    def foldLeft[U](acc: Expr[U])(f: Expr[(U, T) => U])(implicit u: Type[U]): Expr[U] =
      '{ (~list).foldLeft[~u](~acc)(~f) }
  }

  implicit class UnrolledOps[T: Liftable](list: List[T]) {
    def unrolledFoldLeft[U](acc: Expr[U])(f: (Expr[U], T) => Expr[U]): Expr[U] = list match {
      case x :: xs => xs.unrolledFoldLeft(f(acc, x))(f)
      case Nil => acc
    }
  }
}
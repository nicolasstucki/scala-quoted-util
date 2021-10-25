package scala.quoted
package util

/** Literal constant values */
object Consts {
  /** Matches literal sequence of literal constant value expressions and return a sequence of values.
   *
   *  Usage:
   *  ```scala
   *  inline def sum(args: Int*): Int = ${ sumExpr('args) }
   *  def sumExpr(argsExpr: Expr[Seq[Int]])(using Quotes): Expr[Int] = argsExpr match
   *    case Varargs(Consts(args)) =>
   *      // args: Seq[Int]
   *      ...
   *  }
   *  ```
   *
   *  To directly unlift all expressions in a sequence `exprs: Seq[Expr[T]]` consider using `exprs.map(_.unlift)`/`exprs.map(_.unliftOrError)` instead.
   */
  def unapply[T](exprs: Seq[Expr[T]])(using Quotes): Option[Seq[T]] =
    exprs.foldRight(Option(List.empty[T])) { (elem, acc) =>
      (elem, acc) match {
        case (Const(value), Some(lst)) => Some(value :: lst)
        case (_, _) => None
      }
    }
}
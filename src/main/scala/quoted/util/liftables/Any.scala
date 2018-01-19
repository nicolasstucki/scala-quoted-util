package scala.quoted
package util.liftables

object AnyToExpr {
  implicit class LiftAny[T](x: T) extends AnyVal {
    /** Lift the value to an expression */
    def toExpr(implicit liftable: Liftable[T]): Expr[T] = liftable.toExpr(x)
  }
}

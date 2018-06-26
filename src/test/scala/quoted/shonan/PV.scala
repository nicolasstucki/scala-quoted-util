package scala.quoted.shonan

import scala.quoted._

sealed trait PV[T]

case class Sta[T](x: T) extends PV[T]

case class Dyn[T](x: Expr[T]) extends PV[T]

class Dyns[T: Liftable] {
  def dyn(pv: PV[T]): Expr[T] = pv match {
    case Sta(x) => x.toExpr
    case Dyn(x) => x
  }
}

object Dyns {
  val dyni: PV[Int] => Expr[Int] = new Dyns[Int].dyn
}

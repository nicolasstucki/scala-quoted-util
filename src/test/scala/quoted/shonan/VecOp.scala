package scala.quoted.shonan

import scala.quoted._

trait VecOp[Idx, Unt] {
  def iter: Vec[Idx, Unt] => Unt
}

class VecSta extends VecOp[Int, Unit] {
  def iter: Vec[Int, Unit] => Unit = { arr =>
    for (i <- 0 until arr.size)
      arr(i)
  }
  override def toString(): String = s"StaticVec"
}

class VecDyn extends VecOp[Expr[Int], Expr[Unit]] {
  def iter: Vec[Expr[Int], Expr[Unit]] => Expr[Unit] = arr => '{
    for (i <- 0 until ~arr.size)
      ~arr('(i))
  }
  override def toString(): String = s"DynVec"
}

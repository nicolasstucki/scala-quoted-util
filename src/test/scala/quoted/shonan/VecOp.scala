package scala.quoted.shonan

import scala.quoted._

trait VecOp[Idx, Unt] {
  def iter(arr: Vec[Idx, Unt]): Unt
}

class VecSta extends VecOp[Int, Unit] {
  def iter(arr: Vec[Int, Unit]): Unit = {
    for (i <- 0 until arr.size)
      arr(i)
  }
  override def toString(): String = s"StaticVec"
}

class VecDyn extends VecOp[Expr[Int], Expr[Unit]] {
  def iter(arr: Vec[Expr[Int], Expr[Unit]]): Expr[Unit] = '{
    for (i <- 0 until ~arr.size)
      ~arr('(i))
  }
  override def toString(): String = s"DynVec"
}

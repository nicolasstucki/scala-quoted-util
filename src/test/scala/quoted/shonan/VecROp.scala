package scala.quoted.shonan

import scala.quoted._

trait VecROp[Idx, T, Unt] extends VecOp[Idx, Unt] {
  def reduce: ((T, T) => T, T, Vec[Idx, T]) => T
}

class StaticVecR[T](r: Ring[T]) extends VecSta with VecROp[Int, T, Unit] {
  import r._
  def reduce: ((T, T) => T, T, Vec[Int, T]) => T = { (plus, zero, vec) =>
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec(i))
    sum
  }
  override def toString(): String = s"StaticVecR($r)"
}

class DynVecR[T: Type](r: Ring[Expr[T]]) extends VecDyn with VecROp[Expr[Int], Expr[T], Expr[Unit]] {
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = {
    (plus, zero, vec) => '{
      var sum = ~r.zero
      for (i <- 0 until ~vec.size)
        sum = ~{ plus('(sum), vec('(i))) }
      sum
    }
  }
  override def toString(): String = s"DynVecR($r)"
}

class VecRStaDim[T: Type](r: Ring[Expr[T]]) extends VecROp[Int, Expr[T], Expr[Unit]]  {
  val M = new StaticVecR[Expr[T]](r)
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Int, Expr[T]]) => Expr[T] = M.reduce
  val seq: (Expr[Unit], Expr[Unit]) => Expr[Unit] = (e1, e2) => '{ ~e1; ~e2 }
  // val iter:  (arr: Vec[]) = reduce seq .<()>. arr
  def iter(arr: Vec[Int, Expr[Unit]]): Expr[Unit] = {
    def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < arr.size) loop(i + 1, '{ ~acc; ~arr.get(i) })
      else acc
    loop(0, '())
  }
  override def toString(): String = s"VecRStaDim($r)"
}

// class VecRStaDyn[T : Type : Liftable](implicit r: Ring[Expr[T]]) extends VecROp[PV[Int], PV[T], Expr[Unit]] {
//   val VSta = VecROp.dynVecStaDim[T]
//   val VDyn = VecROp.dynVec[T]
//   val dyn = new Dyn[T].dyn
//   def reduce: ((PV[T], PV[T]) => PV[T], PV[T], Vec[PV[Int], PV[T]]) => PV[T] = { (plus, zero, vec) => vec match {
//       case Vec(PV.Sta(n), v) => VSta.reduce(plus, zero, Vec(n, i => v(PV.Sta(i))))
//       case Vec(PV.Dyn(n), v) => VDyn.reduce((x, y) => plus(x, y), zero, Vec(n, i => v(PV.Dyn(i))))
//     }
//   }
//   val seq: (Expr[Unit], Expr[Unit]) => Expr[Unit] = (e1, e2) => '{ ~e1; ~e2 }
//   // val iter:  (arr: Vec[]) = reduce seq .<()>. arr
//   def iter(arr: Vec[Int, Expr[Unit]]): Expr[Unit] = {
//     def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
//       if (i < arr.size) loop(i + 1, '{ ~acc; ~arr.get(i) })
//       else acc
//     loop(0, '())
//   }
//   override def toString(): String = s"VecRStaDim($r)"
// }

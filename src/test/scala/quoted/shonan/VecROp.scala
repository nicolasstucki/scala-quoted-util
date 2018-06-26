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

class VecRDyn[T: Type](r: Ring[Expr[T]]) extends VecDyn with VecROp[Expr[Int], Expr[T], Expr[Unit]] {
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = {
    (plus, zero, vec) => '{
      var sum = ~r.zero
      for (i <- 0 until ~vec.size)
        sum = ~{ plus('(sum), vec('(i))) }
      sum
    }
  }
  override def toString(): String = s"VecRDyn($r)"
}

class VecRStaDim[T: Type](r: Ring[Expr[T]]) extends VecROp[Int, Expr[T], Expr[Unit]]  {
  val M = new StaticVecR[Expr[T]](r)
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Int, Expr[T]]) => Expr[T] = M.reduce
  val seq: (Expr[Unit], Expr[Unit]) => Expr[Unit] = (e1, e2) => '{ ~e1; ~e2 }
  // val iter:  (arr: Vec[]) = reduce seq .<()>. arr
  def iter: Vec[Int, Expr[Unit]] => Expr[Unit] = arr => {
    def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
      if (i < arr.size) loop(i + 1, '{ ~acc; ~arr.get(i) })
      else acc
    loop(0, '())
  }
  override def toString(): String = s"VecRStaDim($r)"
}

class VecRStaDyn[T : Type : Liftable](r: Ring[Expr[T]]) extends VecROp[PV[Int], Expr[T], Expr[Unit]] {
  val VSta = new VecRStaDim(r)
  val VDyn = new VecRDyn(r)
  val dyn = Dyns.dyn[T]
  def reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[PV[Int], Expr[T]]) => Expr[T] = { (plus, zero, vec) => vec match {
      case Vec(Sta(n), v) => VSta.reduce(plus, zero, Vec(n, i => v(Sta(i))))
      case Vec(Dyn(n), v) => VDyn.reduce((x, y) => plus(x, y), zero, Vec(n, i => v(Dyn(i))))
    }
  }
  def iter: Vec[PV[Int], Expr[Unit]] => Expr[Unit] =  arr => {
    arr.size match {
      case Sta(n) =>
        def loop(i: Int, acc: Expr[Unit]): Expr[Unit] =
          if (i < n) loop(i + 1, '{ ~acc; ~arr.get(Sta(i)) })
          else acc
        loop(0, '())
      case Dyn(n) =>
          '{ "TODO"; () }

    }
  }
  override def toString(): String = s"VecRStaDim($r)"
}

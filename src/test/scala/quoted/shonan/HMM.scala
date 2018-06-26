package scala.quoted.shonan

import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

// RINGS

trait Ring[T] {
  val zero: T
  val one: T
  val add: (x: T, y: T) => T
  val sub: (x: T, y: T) => T
  val mul: (x: T, y: T) => T
  // val eq: (x: T, y: T) => Option[Boolean]

  implicit class Ops(x: T) {
    def +(y: T): T = add(x, y)
    def -(y: T): T = sub(x, y)
    def *(y: T): T = mul(x, y)
  }
}

object Ring {
  implicit def ringInt: Ring[Int] = new Ring {
    val zero = 0
    val one  = 0
    val add = (x, y) => x + y
    val sub = (x, y) => x - y
    val mul = (x, y) => x * y
    // val eq  = (x, y) => Some(x == y)
    override def toString(): String = "ringInt"
  }

  implicit def ringIntExpr: Ring[Expr[Int]] = new Ring {
    val zero = '(0)
    val one  = '(1)
    val add = (x, y) => '(~x + ~y)
    val sub = (x, y) => '(~x - ~y)
    val mul = (x, y) => '(~x * ~y)
    // val eq  = (x, y) => None
    override def toString(): String = "ringIntExpr"
  }

  // implicit def ringExpr[U](implicit t: Expr[Ring[U]]): Ring[Expr[U]] = new Ring {
  //   val zero = '((~t).zero)
  //   val one  = '((~t).one)
  //   val add = (x, y) => '((~t).add(~x, ~y))
  //   val sub = (x, y) => '((~t).sub(~x, ~y))
  //   val mul = (x, y) => '((~t).mul(~x, ~y))
  //   // val eq  = (x, y) => None
  // }

  implicit def ringComplex[U](implicit t: Ring[U]): Ring[Complex[U]] = new Ring {
    import t._
    val zero = Complex(t.zero, t.zero)
    val one  = Complex(t.one, t.zero)
    val add = (x, y) => Complex(x.re + y.re, x.im + y.im)
    val sub = (x, y) => Complex(x.re + y.re, x.im + y.im)
    val mul = (x, y) => Complex(x.re * y.re - x.im * y.im, x.re * y.im + x.im * y.re)
    // val eq  = (x, y) => ???
    override def toString(): String = s"ringComplex($t)"
  }

  // implicit def ringComplexExpr[U : Type](implicit t: Ring[U]): Ring[Expr[Complex[U]]] = new Ring[Expr[Complex[U]]] {
  //   import t._
  //   val zero = '(Complex(t.zero, t.zero))
  //   val one  = '(Complex(t.one, t.zero))
  //   val add = (x, y) => '(Complex((~x).re + (~y).re, (~x).im + (~y).im))
  //   val sub = (x, y) => '(Complex((~x).re - (~y).re, (~x).im - (~y).im))
  //   val mul = (x, y) => '(Complex((~x).re * (~y).re - (~x).im * (~y).im, (~x).re * (~y).im + (~x).im * (~y).re))
  //   // val eq  = (x, y) => ???
  // }

  implicit def ringPV[U: Liftable](implicit staRing: Ring[U], dynRing: Ring[Expr[U]]): RingPV[U] = new RingPV[U]() { }

}

case class Complex[T](re: T, im: T)

object Complex {
  implicit def complexIsLiftable[T: Type: Liftable]: Liftable[Complex[T]] = new Liftable {
    def toExpr(c: Complex[T]): Expr[Complex[T]] = '{ Complex(~c.re.toExpr, ~c.im.toExpr) }
  }

 def of_complex_expr(x: Expr[Complex[Int]]): Complex[Expr[Int]] = Complex('((~x).re), '((~x).im))
 def of_expr_complex(x: Complex[Expr[Int]]): Expr[Complex[Int]] = '(Complex(~x.re, ~x.im))

}

class Rings {
  {
    val intComplex = implicitly[Ring[Complex[Int]]]
    import intComplex._

    println(Complex(1, 2) * Complex(4, 2))
  }

  {
    val intExprComplex = implicitly[Ring[Complex[Expr[Int]]]]
    import intExprComplex._

    val res = Complex('(1), '(2)) * Complex('(4), '(2))
    println(s"Complex(${res.re.show}, ${res.im.show})")
  }

  // {
  //   val intExprComplex = implicitly[Ring[Expr[Complex[Int]]]]
  //   import intExprComplex._

  //   val res = '(Complex(1, 2)) * '(Complex(4, 2))
  //   println(res.show)
  // }
}



// VECTORS


case class Vec[Idx, T](size: Idx, get: Idx => T) {
  def apply(idx: Idx): T = get(idx)

  def vecMap[U](f: T => U): Vec[Idx, U] = Vec(size, i => f(get(i)))

  def zipWith[U, V](vec2: Vec[Idx, U], f: (T, U) => V): Vec[Idx, V] =
    Vec(size, i => f(get(i), vec2(i)))
}

case class OVec[Idx, T, Unt](size: Idx, update: (Idx, T) => Unt) {
  def vecAssign(vecIn: Vec[Idx, T]): Vec[Idx, Unt] =
    Vec(vecIn.size, i => update(i, vecIn(i)))
}

object Vec {
  def fromArray[T](a: Array[T]): (Vec[Int, T], OVec[Int, T, Unit]) =
    (Vec(a.size, i => a(i)), OVec(a.size, (i, v) => a(i) = v))
}


// vector ops

trait VecOp[Idx, Unt] {
  def iter(arr: Vec[Idx, Unt]): Unt
}

object VecOp {
  implicit def staticVec: VecOp[Int, Unit] = new StaticVec
  implicit def dynVec: VecOp[Expr[Int], Expr[Unit]] = new DynVec

  class StaticVec extends VecOp[Int, Unit] {
    def iter(arr: Vec[Int, Unit]): Unit = {
      for (i <- 0 until arr.size)
        arr(i)
    }
    override def toString(): String = s"StaticVec"
  }

  class DynVec extends VecOp[Expr[Int], Expr[Unit]] {
    def iter(arr: Vec[Expr[Int], Expr[Unit]]): Expr[Unit] = '{
      for (i <- 0 until ~arr.size)
        ~arr('(i))
    }
    override def toString(): String = s"DynVec"
  }
}


trait VecROp[Idx, T, Unt] extends VecOp[Idx, Unt] {
  def reduce: ((T, T) => T, T, Vec[Idx, T]) => T
}

object VecROp {
  implicit def staticVec[T](implicit r: Ring[T]): VecROp[Int, T, Unit] = new StaticVecR
  implicit def dynVec[T: Type](implicit r: Ring[Expr[T]]): VecROp[Expr[Int], Expr[T], Expr[Unit]] = new DynVecR
  implicit def dynVecStaDim[T: Type](implicit r: Ring[Expr[T]]): VecROp[Int, Expr[T], Expr[Unit]] = new VecRStaDim

  class StaticVecR[T](implicit r: Ring[T]) extends VecOp.StaticVec with VecROp[Int, T, Unit] {
    import r._
    def reduce: ((T, T) => T, T, Vec[Int, T]) => T = { (plus, zero, vec) =>
      var sum = zero
      for (i <- 0 until vec.size)
        sum = plus(sum, vec(i))
      sum
    }
    override def toString(): String = s"StaticVecR($r)"
  }

  class DynVecR[T: Type](implicit r: Ring[Expr[T]]) extends VecOp.DynVec with VecROp[Expr[Int], Expr[T], Expr[Unit]] {
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

  class VecRStaDim[T: Type](implicit r: Ring[Expr[T]]) extends VecROp[Int, Expr[T], Expr[Unit]]  {
    val M = VecROp.staticVec[Expr[T]]
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

  class VecRStaDyn[T : Type : Liftable](implicit r: Ring[Expr[T]]) extends VecROp[PV[Int], PV[T], Expr[Unit]] {
    val VSta = VecROp.dynVecStaDim[T]
    val VDyn = VecROp.dynVec[T]
    val dyn = new Dyn[T].dyn
    def reduce: ((PV[T], PV[T]) => PV[T], PV[T], Vec[PV[Int], PV[T]]) => PV[T] = { (plus, zero, vec) => vec match {
        case Vec(PV.Sta(n), v) => VSta.reduce(plus, zero, Vec(n, i => v(PV.Sta(i))))
        case Vec(PV.Dyn(n), v) => VDyn.reduce((x, y) => plus(x, y), zero, Vec(n, i => v(PV.Dyn(i))))
      }
    }
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

}

// blas

class Blas1[Idx, T, Unt](implicit r: Ring[T], vec: VecOp[Idx, Unt]) {
  import r._
  import vec._

  implicit class Blas1VecOps(v1: Vec[Idx, T]) {
    def `*.`(v2: Vec[Idx, T]): Vec[Idx, T] = v1.zipWith(v2, mul)
  }

  implicit class Blas1OVecOps(vout: OVec[Idx, T, Unt]) {
    def :=(vin: Vec[Idx, T]): Unt = iter(vout.vecAssign(vin))
  }
  override def toString(): String = s"Blas1($r, $vec)"
}

class Blas2[Idx, T, Unt](implicit r: Ring[T], vec: VecROp[Idx, T, Unt]) extends Blas1[Idx, T, Unt] {
  import r._
  import vec._

  implicit class Blas2VecOps(v1: Vec[Idx, T]) {
    def dot(v2: Vec[Idx, T]): T = reduce(add, zero, v1 `*.` v2)
  }

  implicit class Blas2MatOps(a: Vec[Idx, Vec[Idx, T]]) {
    def *(v: Vec[Idx, T]): Vec[Idx, T] = a.vecMap(x => v dot x)
  }
  override def toString(): String = s"Blas2($r, $vec)"
}

// vmult

class Vmult[Idx, T, Unt](implicit r: Ring[T], vec: VecOp[Idx, Unt]) {
  private[this] val blas = new Blas1()(r, vec)
  import blas._
  def vmult(vout: OVec[Idx, T, Unt], v1: Vec[Idx, T], v2: Vec[Idx, T]): Unt = vout := v1 `*.` v2
  override def toString(): String = s"Vmult($r, $vec)"
}

object Vmults {
  def vmult(vout: Array[Complex[Int]], v1: Array[Complex[Int]], v2: Array[Complex[Int]]): Unit = {
    val n = vout.length

    val vout_ = OVec(n, (i, v: Complex[Int]) => vout(i) = v)
    val v1_ = Vec (n, i => v1(i))
    val v2_ = Vec (n, i => v2(i))

    val V = new Vmult[Int, Complex[Int], Unit]
    V.vmult(vout_, v1_, v2_)
  }

  def vmultCA: Expr[(Array[Complex[Int]], Array[Complex[Int]], Array[Complex[Int]]) => Unit] = '{
    (vout, v1, v2) => {
      val n = vout.length
      ~{
        val vout_ = OVec[Expr[Int], Complex[Expr[Int]], Expr[Unit]]('(n), (i, v) => '(vout(~i) = ~Complex.of_expr_complex(v)))
        val v1_ = Vec ('(n), i => Complex.of_complex_expr('(v1(~i))))
        val v2_ = Vec ('(n), i => Complex.of_complex_expr('(v2(~i))))

        val V = new Vmult[Expr[Int], Complex[Expr[Int]], Expr[Unit]]
        V.vmult(vout_, v1_, v2_)
      }
    }
  }
}


class MVmult[Idx, T, Unt](implicit r: Ring[T], vec: VecROp[Idx, T, Unt]) {
  private[this] val blas2 = new Blas2()(r, vec)
  import blas2._
  def mvmult(vout: OVec[Idx, T, Unt], a: Vec[Idx, Vec[Idx, T]], v: Vec[Idx, T]): Unt = vout := a * v
  override def toString(): String = s"MVmult($r, $vec)"
}

object MVmult {
  def mvmult_p(vout: Array[Int], a: Array[Array[Int]], v: Array[Int]): Unit = {
    val n = vout.length
    val m = v.length

    val vout_ = OVec(n, (i, x: Int) => vout(i) = x)
    val a_ = Vec (n, i => Vec(m, j => a(i)(j)))
    val v_ = Vec (n, i => v(i))

    val MV = new MVmult[Int, Int, Unit]
    MV.mvmult(vout_, a_, v_)
  }

  def mvmult_c: Expr[(Array[Int], Array[Array[Int]], Array[Int]) => Unit] = '{
    (vout, a, v) => {
      val n = vout.length
      val m = v.length
      ~{
        val vout_ = OVec('(n), (i, x: Expr[Int]) => '(vout(~i) = ~x))
        val a_ = Vec('(n), (i: Expr[Int]) => Vec('(m), (j: Expr[Int]) => '{ a(~i)(~j) } ))
        val v_ = Vec('(m), (i: Expr[Int]) => '(v(~i)))

        val MV = new MVmult[Expr[Int], Expr[Int], Expr[Unit]]
        MV.mvmult(vout_, a_, v_)
      }
    }
  }

  def mvmult_mc(n: Int, m: Int): Expr[(Array[Int], Array[Array[Int]], Array[Int]) => Unit] = {
    val MV = new MVmult[Int, Expr[Int], Expr[Unit]]
    '{
      (vout, a, v) => {
        assert (~n.toExpr == vout.length && ~m.toExpr == v.length)
        ~{
          val vout_ = OVec(n, (i, x: Expr[Int]) => '(vout(~i.toExpr) = ~x))
          val a_ = Vec(n, i => Vec(m, j => '{ a(~i.toExpr)(~j.toExpr) } ))
          val v_ = Vec(m, i => '(v(~i.toExpr)))

          MV.mvmult(vout_, a_, v_)
        }
      }
    }
  }

  def mvmult_ac(a: Array[Array[Int]]): Expr[(Array[Int], Array[Int]) => Unit] = {
    val n = a.length
    val m = a(0).length
    import util.Lifters._
    '{
      val arr = ~a.toExpr
      (vout, v) => {
        assert (~n.toExpr == vout.length && ~m.toExpr == v.length)
        ~{
          val vout_ : OVec[PV[Int], PV[Int], Expr[Unit]] = OVec(PV.Sta(n), (i, x: PV[Int]) => '(vout(~Dyn.dyni(i)) = ~Dyn.dyni(x)))
          val a2: Vec[PV[Int], Vec[PV[Int], PV[Int]]] = Vec(PV.Sta(n), i => Vec(PV.Sta(m), j => (i, j) match {
            case (PV.Sta(i), PV.Sta(j)) => PV.Sta(a(i)(j))
            case (PV.Sta(i), PV.Dyn(j)) => PV.Dyn('(arr(~i.toExpr)(~j)))
            case (i, j) => PV.Dyn('{ arr(~(Dyn.dyni(i)))(~(Dyn.dyni(j))) })
          }))
          val v_ : Vec[PV[Int], PV[Int]] = Vec(PV.Sta(m), i => PV.Dyn('(v(~Dyn.dyni(i)))))
          val RingFloatPCode = new RingPV[Int]() {}
          val MV = new MVmult[PV[Int], PV[Int], Expr[Unit]]()(RingFloatPCode, new VecROp.VecRStaDyn[Int](){})
          MV.mvmult(vout_, a2, v_)
        }
      }
    }
  }

    // let n = Array.length a in
    // let m = Array.length a.(0) in
    // let a = Vec (Sta n, fun i → Vec (Sta m,
    // (fun j →
    // match (i,j) with
    // | (Sta i, Sta j) → Sta a.(i).(j)
    // | (Sta i, Dyn j) → Dyn .<a.(i).(.~j)>.
    // | (i, j) → Dyn .<a.(.~(dyni i)).(.~(dyni j))>.))) in
    // .<fun vout v →
    // assert (n = Array.length vout && m = Array.length v);
    // .~(let vout = OVec (Sta n, fun i v → .<vout.(.~(dyni i)) ← .~(dynf v)>.) in
    // let v = Vec (Sta m, fun j → Dyn .<v.(.~(dyni j))>.) in
    // let module MV = MVMULT(RingFloatPCode)(VecRStaDyn(Lift_float))
}


// DYNAMIC

sealed trait PV[T]
object PV {
  case class Sta[T](x: T) extends PV[T]
  case class Dyn[T](x: Expr[T]) extends PV[T]
}

class Dyn[T: Liftable] {
  def dyn(pv: PV[T]): Expr[T] = pv match {
    case PV.Sta(x) => x.toExpr
    case PV.Dyn(x) => x
  }
}

object Dyn {
  val dyni: PV[Int] => Expr[Int] = new Dyn[Int].dyn
}


trait RingPV[U: Liftable](implicit staRing: Ring[U], dynRing: Ring[Expr[U]]) extends Ring[PV[U]] {
  type T = PV[U]

  val dyn = new Dyn[U].dyn

  val zero: T = PV.Sta(staRing.zero)
  val one: T = PV.Sta(staRing.one)
  val add = (x: T, y: T) => (x, y) match {
    case (PV.Sta(x), PV.Sta(y)) => PV.Sta(staRing.add(x,y))
    case (x, y) => PV.Dyn(dynRing.add(dyn(x), dyn(y)))
  }
  val sub = (x: T, y: T) => (x, y) match {
    case (PV.Sta(x), PV.Sta(y)) => PV.Sta(staRing.sub(x,y))
    case (x, y) => PV.Dyn(dynRing.sub(dyn(x), dyn(y)))
  }
  val mul = (x: T, y: T) => (x, y) match {
    case (PV.Sta(x), PV.Sta(y)) => PV.Sta(staRing.mul(x,y))
    case (x, y) => PV.Dyn(dynRing.mul(dyn(x), dyn(y)))
  }
  // val eq: (x: T, y: T) => Option[Boolean]

  implicit class RingPVOps(x: T) {
    def +(y: T): T = add(x, y)
    def -(y: T): T = sub(x, y)
    def *(y: T): T = mul(x, y)
  }
}


// module RingPV(STA:RING)(DYN:RING with type t = STA.t code)
// (L:LIFT with type t = STA.t) =
// struct
// type t = STA.t pv
// include Dyn(L)
// let zero = Sta STA.zero
// let one = Sta STA.one
// let add x y =
// match (x,y) with
// | (Sta x, Sta y) → Sta STA.(add x y)
// | (x, y) → Dyn DYN.(add (dyn x) (dyn y))
// let sub x y = . . .
// let mul x y = . . .
// end

object HMM {

  def main(args: Array[String]): Unit = {
    println("dafd")
    new Rings

    val arr1 = Array(Complex(1, 0), Complex(0, 4), Complex(2, 2))
    val arr2 = Array(Complex(2, 0), Complex(1, 1), Complex(1, 2))
    val out  = Array(Complex(0, 0), Complex(0, 0), Complex(0, 0))
    Vmults.vmult(out, arr1, arr2)
    println(out.toList)

    println(Vmults.vmultCA.show)

    val a = Array(
      Array( 5,  0,  0,  5,  0),
      Array( 0,  0, 10,  0,  0),
      Array( 0, 10,  0,  0,  0),
      Array( 0,  0,  2,  3,  5),
      Array( 0,  0,  3,  0,  7)
    )

    val v1 = Array(1, 2, 3, 4, 5)
    val v1out = Array(0, 0, 0, 0, 0)
    MVmult.mvmult_p(v1out, a, v1)
    println(v1out.toList)
    println()
    println()
    println()

    println(MVmult.mvmult_c.show)
    println()
    println()
    println()

    println(MVmult.mvmult_mc(3, 2).show)
    println()
    println()
    println()

    println(MVmult.mvmult_ac(a).show)
    println()
    println()
    println()

  }
}




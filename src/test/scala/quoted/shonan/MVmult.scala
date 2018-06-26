package scala.quoted.shonan

import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

class MVmult[Idx, T, Unt](tring: Ring[T], vec: VecROp[Idx, T, Unt]) {
  private[this] val blas2 = new Blas2(tring, vec)
  import blas2._
  def mvmult(vout: OVec[Idx, T, Unt], a: Vec[Idx, Vec[Idx, T]], v: Vec[Idx, T]): Unt = vout := a * v
  override def toString(): String = s"MVmult($tring, $vec)"
}

object MVmult {
  def mvmult_p(vout: Array[Int], a: Array[Array[Int]], v: Array[Int]): Unit = {
    val n = vout.length
    val m = v.length

    val vout_ = OVec(n, (i, x: Int) => vout(i) = x)
    val a_ = Vec (n, i => Vec(m, j => a(i)(j)))
    val v_ = Vec (n, i => v(i))

    val MV = new MVmult[Int, Int, Unit](RingInt, new StaticVecR(RingInt))
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

        val MV = new MVmult[Expr[Int], Expr[Int], Expr[Unit]](RingIntExpr, new VecRDyn(RingIntExpr))
        MV.mvmult(vout_, a_, v_)
      }
    }
  }

  def mvmult_mc(n: Int, m: Int): Expr[(Array[Int], Array[Array[Int]], Array[Int]) => Unit] = {
    val MV = new MVmult[Int, Expr[Int], Expr[Unit]](RingIntExpr, new VecRStaDim(RingIntExpr))
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
          val vout_ : OVec[PV[Int], Expr[Int], Expr[Unit]] = OVec(Sta(n), (i, x) => '(vout(~Dyns.dyni(i)) = ~x))
          val a2: Vec[PV[Int], Vec[PV[Int], Expr[Int]]] = Vec(Sta(n), i => Vec(Sta(m), j => Dyns.dyn((i, j) match {
            case (Sta(i), Sta(j)) => Sta(a(i)(j))
            case (Sta(i), Dyn(j)) => Dyn('(arr(~i.toExpr)(~j)))
            case (i, j) => Dyn('{ arr(~(Dyns.dyni(i)))(~(Dyns.dyni(j))) })
          })))
          val v_ : Vec[PV[Int], Expr[Int]] = Vec(Sta(m), i => '(v(~Dyns.dyni(i))))
          val MV = new MVmult[PV[Int], Expr[Int], Expr[Unit]](RingIntExpr, new VecRStaDyn(RingIntExpr))
          MV.mvmult(vout_, a2, v_)
        }
      }
    }
  }
}

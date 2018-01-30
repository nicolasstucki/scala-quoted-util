package scala.quoted
package util

import scala.quoted.Liftable._
import scala.quoted.util.Lifters._

object Blocks {

  def block[T](stats: Seq[Expr[_]], expr: Expr[T]): Expr[T] = stats match {
    case Seq() => expr
    case Seq(x1) => '{ ~x1; ~expr }
    case Seq(x1, x2) => '{ ~x1; ~x2; ~expr }
    case Seq(x1, x2, x3) => '{ ~x1; ~x2; ~x3; ~expr }
    case Seq(x1, x2, x3, x4) => '{ ~x1; ~x2; ~x3; ~x4; ~expr }
    case _ => // TODO make tree shallower
      val (stats1, stats2) = stats.splitAt(stats.length / 2)
      '{ ~block(stats1, ()); ~block(stats2, expr) }
  }

}

package preprocessing

import other.Types._
import math._

object Illumination {

  /** x => 128*x*x/localEX2(x, s, s) */
  def illumination(mat: MInt, s: Int): MInt = {
    val m = mat.length; val n = mat(0).length
    val MX2 = postprocessing.Statistic.localEX2(mat, s, s)
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) =
        if (MX2(y)(x) != 0) (sqr(mat(y)(x)) << 8) / MX2(y)(x)
        else 0
    res
  }

  /** x => 128*x*x/localEX2(x, s, s) */
  def illumination(mat: M, s: Int): M = {
    val m = mat.length; val n = mat(0).length
    val MX2 = postprocessing.Statistic.localEX2(mat, s, s)
    val res = createM(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) =
        if (MX2(y)(x) != 0) 128.0 * sqr(mat(y)(x)) / MX2(y)(x)
        else 0
    res
  }
}
package preprocessing

import basic.Basic._
import math._

object Alignment {
  
  def illumination(mat: MInt, s: Int): MInt = {
    val m = mat.length; val n = mat(0).length
    val MX2 = basic.Statistic.mx2Mat(mat, s, s)
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) =
        if (MX2(y)(x) != 0) (sqr(mat(y)(x)) << 8)/MX2(y)(x)
        else 0
    res
  }
  
  def illumination(mat: M, s: Int): M = {
    val m = mat.length; val n = mat(0).length
    val MX2 = basic.Statistic.mx2Mat(mat, s, s)
    val res = createM(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) = 
        if (MX2(y)(x)!=0) 128.0*sqr(mat(y)(x))/MX2(y)(x)
        else 0
    res
  }
}
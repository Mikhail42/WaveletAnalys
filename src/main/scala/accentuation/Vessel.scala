package accentuation

import math._
import other.Types._
import image._

class Vessel(mat: MInt, d1: Int, s: Int, stepTheta: Int = 10, border: Int = 128) {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** NOTE: In this class, I use Array, so array has a fastest method apply(i: Int)
   */

  private val m = mat.length;
  private val n = mat(0).length

  private val thetas = (0 until 180 by stepTheta).toArray

  private val (lr1, rr1) = (d1 / 2, d1 / 2)
  private val (lr2, rr2) = (lr1 + 2, rr1 + 2)
  private val rs = (-lr2 to rr2).toArray

  private val core: Array[Int] = {
    logger.debug(s"create core with leftRadius1=${lr1} and rightRadius1=${rr1}")
    val core = rs.map {
      r =>
        if (r == 0) 0
        else if (r >= -lr1 && r <= rr1) 2
        else if (r > 0) -rr1
        else -lr1
    }
    assert { core.sum == 0 }
    core.toArray
  }

  private val (rdx, rdy): (MInt, MInt) = {
    logger.debug(s"rs.length * thetas.length is ${thetas.length * rs.length}")
    val rdy = Array.ofDim[Int](rs.length, thetas.length)
    val rdx = Array.ofDim[Int](rs.length, thetas.length)
    for (i <- 0 until rs.length) {
      val r = rs(i)
      for (j <- 0 until thetas.length) {
        rdy(i)(j) = deltaY(r, thetas(j))
        rdx(i)(j) = deltaX(r, thetas(j))
      }
    }
    (rdx, rdy)
  }

  // 2*s*m*n
  //private lazy val matUpd = preprocessing.Illumination.illumination(mat, s)
  //preprocessing.Inverse.inverse(matUpd)

  private def localMask(i: Int, j: Int, thetaInd: Int): Int = {
    var sum: Int = 0
    for (rInd <- 0 until rs.length)
      sum += mat(i + rdy(rInd)(thetaInd))(j + rdx(rInd)(thetaInd)) * core(rInd)
    sum
  }

  def accentuation(): (MInt, MInt, MInt) = {
    logger.debug(s"accentuation vessel on matix started with diameter=${d1}")

    val resTranform: MInt = createMInt(m, n)
    val trueDir: MInt = createMInt(m, n)
    // 'par' is used for speedup in 2-3 times on my PC
    // My PC characters: 8 cores, Intel Core i7, 8GB RAM, GNU/Linux Debian 9. Current scalaVersion: 2.12.2
    (lr2 until m - rr2).par.map {
      i =>
        for (j <- lr2 until n - rr2) {
          val (trans, dir) = maxSearch(i, j)
          resTranform(i)(j) = if (trans < 128) 0 else 255
          trueDir(i)(j) = dir
        }
    }

    val res = resTranform.map { _.map { _.toInt } }
    postprocessing.Mediate.mediate(trueDir, res)

    (resTranform, trueDir, res)
  }

  private def maxSearch(i: Int, j: Int): (Int, Int) = {
    var mx = Int.MinValue
    var dir = 0
    for (thetaInd <- 0 until thetas.length) {
      val lm = localMask(i, j, thetaInd)
      if (mx < lm) {
        mx = lm
        dir = thetas(thetaInd)
      }
    }
    (mx, dir)
  }

  private def minSearch(i: Int, j: Int): (Int, Int) = {
    var mx = Int.MaxValue
    var dir = 0
    for (thetaInd <- 0 until thetas.length) {
      val lm = localMask(i, j, thetaInd)
      if (mx > lm) {
        mx = lm
        dir = thetas(thetaInd)
      }
    }
    (mx, dir)
  }
}
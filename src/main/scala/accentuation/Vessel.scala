package accentuation

import math._
import other.Types._
import image._

class Vessel(mat: MInt, d1: Int, s: Int, stepTheta: Int = 10, border: Int = 128) {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  private val m = mat.length;
  private val n = mat(0).length

  private val thetas = (0 until 180 by stepTheta)

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
  //val matUpd = preprocessing.Illumination.illumination(mat, s)

  def locMask(i: Int, j: Int, thetaInd: Int): Int = {
    var sum: Int = 0
    for (rInd <- 0 until rs.length)
      sum += mat(i + rdy(rInd)(thetaInd))(j + rdx(rInd)(thetaInd)) * core(rInd)
    sum
  }

  def accent: (MInt, MInt, MInt) = {
    logger.debug(s"accentuation vessel on matix started with diameter=${d1}")

    val resTranform: MInt = createMInt(m, n)
    //val trueDir: MInt = createMInt(m, n)
    (lr2 until m - rr2).par.map {
      i =>
        for (j <- lr2 until n - rr2) {
          val transAndDir = maxSearch(i, j)
          resTranform(i)(j) = if (transAndDir._1 < 128) 0 else 255
          //trueDir(i)(j) = transAndDir._2
        }
    }

    //val res = resTranform.map { _.map { _.toInt } }
    //postprocessing.Mediate.mediate(trueDir, res)

    (resTranform, null, null)
  }

  private def maxSearch(i: Int, j: Int): (Int, Int) = {
    var mx = Int.MinValue
    var dir = 0
    for (thetaInd <- 0 until thetas.length) {
      val lm = locMask(i, j, thetaInd)
      if (mx < lm) {
        mx = lm
        dir = thetas(thetaInd)
      }
    }
    (mx, dir)
  }
}
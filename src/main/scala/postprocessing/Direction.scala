package postprocessing

import math._
import java.awt.image._

import main.Basic._
import main.Constants._

object Direction {
  /** search direction matrix
   *  use a parallel mapping (outer cycle)
   *  @param mat -- matrix to search direction
   *  @param R -- radius for each point, must be non negative integer
   *  @param steptheta -- step of theta, must be positive integer
   *  @return direction matrix
   */
  def direction(mat: MInt, R: Int = 5, steptheta: Int = 5, extr: String = "ARGMIN"): MInt = {
    val m = mat.length; val n = mat(0).length
    val rs = (-R to R)
    def dys(theta: Int) = rs.map { r => deltaY(r, theta) }
    def dxs(theta: Int) = rs.map { r => deltaX(r, theta) }
    val allDYs = (0 until 180 by steptheta).map { dys(_) }
    val allDXs = (0 until 180 by steptheta).map { dxs(_) }

    val res = createMInt(m, n)
    (0 until m).par.map {
      i =>
        for (j <- 0 until n)
          res(i)(j) = argDisp(i, j)
    }

    def getDisp(i: Int, j: Int, theta: Int): T = {
      val ind = theta / steptheta
      val DYs = allDYs(ind); val DXs = allDXs(ind)
      val ys = DYs.map { dy => (i + dy).max(0).min(m - 1) }
      val xs = DXs.map { dx => (j + dx).max(0).min(n - 1) }
      val myx = (0 to 2 * R).map { i => mat(ys(i))(xs(i)) }
      val mx = myx.sum
      val mx2 = myx.map { x => x * x }.sum
      val s = (2 * R.toInt + 1).toDouble
      (mx2.toDouble / s - (mx * mx).toDouble / (s * s))
    }

    def argDisp =
      if (extr == "ARGMIN") argMinDisp(_, _)
      else argMaxDisp(_, _)

    def argMinDisp(i: Int, j: Int): Int = {
      val disps = (0 until 180 by steptheta).map { getDisp(i, j, _) }
      disps.indexOf(disps.min) * steptheta
    }

    def argMaxDisp(i: Int, j: Int): Int = {
      val disps = (0 until 180 by steptheta).map { getDisp(i, j, _) }
      disps.indexOf(disps.max) * steptheta
    }

    val medRes = createMInt(m, n)
    (1 until m - 1).par.map {
      i =>
        for (j <- 1 until n - 1)
          medRes(i)(j) = locMed(i, j)
    }

    def locMed(i: Int, j: Int): Int = {
      var sum = 0
      for (dy <- -1 to 1; dx <- -1 to 1)
        sum += res(i + dy)(j + dx)
      sum / 9
    }

    medRes
  }

  /** search a image direction */
  def direction(img: BI): BI = intMatToImg(direction(imgToMInt(img)))

}
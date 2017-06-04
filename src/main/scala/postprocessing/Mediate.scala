package postprocessing

import math._
import java.awt.image._

import other.Types._
import other.Constants._

object Mediate {
  val white = 20

  def mediate(directly: MInt, mat: MInt): MInt = {
    val m = directly.length; val n = directly(0).length

    val mediateMat = createMBool(m, n)
    for (y <- 0 until m; x <- 0 until n)
      if (mat(y)(x) > white) {
        val (yMed, xMed): (Int, Int) =
          getMediateLine(mat, x, y, directly(y)(x))
        if (sqr(yMed - y) + sqr(xMed - x) <= 2)
          mediateMat(yMed)(xMed) = true
      }

    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) =
        if (!mediateMat(y)(x)) 0
        else 255
    res
  }

  /** @param imgMat -- components of image, access to element in format (h,w)
   *  @param angle -- (directly of blood vessel - pi/2), in [-pi/2, pi/2).
   *  @param x -- current value pixel's width
   *  @param y -- current value pixel's height
   *  @return (r1, r2),
   *    where #r1 -- radius of the motion in the direction of the #angle
   *      from the point (#x, #y) until the last black dot in, #r2 -- in the direction of the (#angle+Pi).
   */
  def radsWhiteLine(imgMat: MInt, theta: Int, x: Int, y: Int): (Int, Int) = {
    val m = imgMat.length; val n = imgMat(0).length
    /** @see image.ImageWaveletInterface.white */
    def isWhite(x: Int, y: Int) = imgMat(y)(x) > white
    def inMatrix(x: Int, y: Int) = (x >= 0 && y >= 0 && x < n && y < m)
    def r(theta: Int): Int = {
      val angle = theta * Pi / 180
      val cosA = cos(angle); val sinA = sin(angle)
      var r = 0
      var kx = x; var ky = y
      while (inMatrix(kx, ky) && isWhite(kx, ky)) {
        kx = x + toInt(r * cosA)
        ky = y + toInt(r * sinA)
        r += 1
      }
      r
    }

    (r(theta) - 1, r(theta + 180) - 1)
  }

  def lengthWhiteLine(imgMat: MInt, theta: Int, x: Int, y: Int): Int = {
    val rads = radsWhiteLine(imgMat, theta, x, y)
    rads._2 + rads._1
  }

  /** @param imgMat -- components of image, access to element in format (h,w)
   *  @param theta -- directly of line, in [0, 180).
   *  @param x -- current value pixel's width
   *  @param y -- current value pixel's height
   *  @return (yMediate, xMediane) -- coordiane of mediate white line:
   *      (theta + 90[degree] = directly(line)) & ((y,x) in line)
   */
  def getMediateLine(imgMat: MInt, x: Int, y: Int, theta: Int): (Int, Int) = {
    val (r1, r2) = radsWhiteLine(imgMat, theta + 90, x, y)
    val cosA = coss(theta + 90); val sinA = sins(theta + 90)

    val xMed = x + toInt((r1 - r2) * cosA / 2)
    val yMed = y + toInt((r1 - r2) * sinA / 2)

    (yMed, xMed)
  }

}
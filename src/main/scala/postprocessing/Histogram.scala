package postprocessing

import math._
import java.awt.image._

import other.Types._
import other.Constants._

object Histogram {

  def histogram(img: BI): Array[Int] = {
    val res = new AInt(256)
    val mat = imgToMInt(img)
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      res(mat(i)(j)) += 1
    res
  }

  private def twoMaxHistogramtInds(gi: AInt) = {
    val absMax = gi.max
    val indAbsMax = gi.indexOf(absMax)
    val sum1 = (5 to indAbsMax).map { gi(_) }.sum
    val sum2 = (indAbsMax until 250).map { gi(_) }.sum
    val indLocMax =
      if (sum1 > sum2) {
        val locMax = (5 to 3 * indAbsMax / 5).map { gi(_) }.max
        5 + (5 to 3 * indAbsMax / 5).map { gi(_) }.indexOf(locMax)
      } else {
        val locMax = (indAbsMax + (250 - indAbsMax) * 2 / 5 until 250).map { gi(_) }.max
        indAbsMax + (indAbsMax + (250 - indAbsMax) * 2 / 5 until 250).map { gi(_) }.indexOf(locMax)
      }

    (min(indLocMax, indAbsMax), max(indLocMax, indAbsMax))
  }

  private def minHistogramInd(gi: AInt) = {
    val (ind1, ind2) = twoMaxHistogramtInds(gi)
    println(ind1, ind2)
    val mn = (ind1 to ind2).map { gi(_) }.min
    ind1 + (ind1 to ind2).map { gi(_) }.indexOf(mn)
  }

  def minHistogram(img: BI): Int = {
    val gi = histogram(img)
    minHistogramInd(gi)
  }
}
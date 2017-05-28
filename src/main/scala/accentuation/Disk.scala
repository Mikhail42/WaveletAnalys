package accentuation

import basic.Basic._
import image._
import math._

object Disk {

  private val L: T = 3
  private val sigma: T = 5
  private val invSigma2: T = 1.0 / (sigma * sigma)

  /** disk emphasis
   *  use a #colorId color-components as default
   *  for use other color-components, use function #disk(mat: M, _)
   *  @param img: image to accentuation disc-like objects
   *  @param r -- radius of objects
   *  @see #emphasis(mat: M, _)
   *  @return image with emphasis object
   */
  def emphasis(img: BI, r: Int): BI = {
    val mat = mapIT(Input.getColorsComponents(img, colorId), toDouble)
    val resMat = emphasis(mat, r)
    Operation.matrixToImage(resMat, img.getType)
  }

  /** disks emphasis
   *  @param mat: matrix to emphasis disc-like objects
   *  @param r -- radius of objects
   *  @see #emphasis(img: BI, _)
   *  @return matrix with emphasis object
   */
  def emphasis(mat: M, r: Int): M = {
    val m = mat.length; val n = mat(0).length
    // L*L * exp(-invSigma2 * (x * x + y * y))
    val ls = (0 to r).map { x => L * exp(-invSigma2 * (x * x)) }
    val sumL = ls.sum - ls(0)
    val R = r + 1
    val strSum = createM(m, n)
    for (y <- R until m - R; x <- R until n - R) {
      strSum(y)(x) = mat(y)(x) * ls(0)
      for (p <- 1 to r)
        strSum(y)(x) += (mat(y)(x + p) + mat(y)(x - p)) * ls(p)
      strSum(y)(x) -= (mat(y)(x + R) + mat(y)(x - R)) * sumL
    }
    val colSum = createM(m, n)
    for (y <- R until m - R; x <- R until n - R) {
      colSum(y)(x) = strSum(y)(x) * ls(0)
      for (p <- 1 to r)
        colSum(y)(x) += (strSum(y + p)(x) + strSum(y - p)(x)) * ls(p)
      colSum(y)(x) -= (strSum(y + R)(x) + strSum(y - R)(x)) * sumL
    }
    colSum
  }
}
package accentuation

import basic.Basic._
import image._
import math._

object Disk {

  private val L: T = 8
  private val sigma: T = 5
  private val invSigma2: T = 1.0 / (sigma * sigma)

  /** disk emphasis
   *  use a #colorId color-components as default
   *  for use other color-components, use function #disk(mat: M, _)
   *  @param img: image to accentuation disc-like objects
   *  @param r -- radius of objects
   *  @see #disk(mat: M, _)
   *  @return image with accentuation's object-as-disk
   */
  def emphasis(img: BI, r: Int): BI = {
    val mat = mapIT(Input.getColorsComponents(img, colorId), toDouble)
    val resMat = emphasis(mat, r)
    Operation.matrixToImage(resMat, img.getType)
  }

  /** disks emphasis
   *  use a parallel map
   *
   *  @param mat: matrix to accentuation disc-like objects
   *  @param r -- radius of objects
   *  @see #disk(img: BI, _)
   *  @return matrix with accentuation's object-as-disk
   */
  def emphasis(mat: M, r: Int): M = {
    val m = mat.length; val n = mat(0).length
    val r2 = r * r

    val innerDisk: M = {
      val res = createM(2 * r + 1, 2 * r + 1)
      for (y <- -r to r; x <- -r to r)
        if (x * x + y * y <= r2)
          res(y + r)(x + r) = L * exp(-invSigma2 * (x * x + y * y))
      res
    }

    val norm: T = innerDisk.map { _.sum }.sum
    val R = r + 2
    val R2 = R * R
    var count = 0
    for (y <- 0 until 2 * R + 1; x <- 0 until 2 * R + 1) {
      val _r2_ = y * y + x * x
      if (_r2_ > r2 && _r2_ <= R2)
        count += 1
    }
    val anti = -norm.toDouble / count

    val outDisk = createM(2 * R + 1, 2 * R + 1)
    for (y <- 0 until 2 * R + 1; x <- 0 until 2 * R + 1) {
      val _r2_ = y * y + x * x
      if (_r2_ <= r2)
        outDisk(y + R)(x + R) = innerDisk(y + r)(x + r)
      else if (_r2_ <= R2)
        outDisk(y + R)(x + R) = anti
    }

    def locMask(i: Int, j: Int): T = {
      var sum: T = 0
      for (y <- -R to R; x <- -R to R)
        sum += mat(y + i)(x + j) * outDisk(y + R)(x + R)
      sum
    }

    val res = createM(m, n)
    (R until m - R).par.map {
      i =>
        for (j <- R until n - R)
          res(i)(j) = mat(i)(j) * locMask(i, j)
    }

    res
  }
}
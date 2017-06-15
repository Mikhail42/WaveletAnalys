package postprocessing

import math._
import other.Types._

/** For test see [[test.StatisticSpec]]
 */
object Statistic {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** approximate in 2 times faster then simple min and max cuncurently */
  def minMax(mat: M) = {
    logger.info(s"min-max of matrix started")
    var min = mat(0)(0)
    var max = mat(0)(0)
    for (str <- mat; x <- str) {
      if (x < min) min = x
      else if (x > max) max = x
    }
    (min, max)
  }

  /** approximate in 2 times faster then simple min and max cuncurently */
  def minMax(mat: MInt) = {
    logger.info(s"min-max of matrix started")
    var min = mat(0)(0)
    var max = mat(0)(0)
    for (str <- mat; x <- str) {
      if (x < min) min = x
      else if (x > max) max = x
    }
    (min, max)
  }

  /** X => EX^2-(EX)^2 */
  def disp(mat: M, aver: T): T = {
    logger.info(s"dispertion of matrix started")
    mat.map { _.map { y => y * y }.sum }.sum /
      productSize(mat) - aver * aver
  }

  /** X => EX */
  def aver(mat: M): T = mat.map { _.sum }.sum / productSize(mat)

  /** (X,Y) => E[X.*Y] - EX*EY
   *  @see {@link StatisticSpec}
   */
  def correlation(mat1: M, mat2: M): T = {
    logger.info(s"correlation two matrix started")
    val aver1 = aver(mat1); val aver2 = aver(mat2)
    var sum: T = 0
    for (i <- 0 until mat1.length; j <- 0 until mat1(0).length)
      sum += mat1(i)(j) * mat2(i)(j)
    sum / productSize(mat1) - aver1 * aver2
  }

  /** @param sy -- half size on hight
   *  @param sx -- half size on weight
   *  def: isNorm(z) = (z>=sz)&&(z<maxZ-sz)
   *  mat(y)(x) => averange in window mat(y-sy:y+sy)(x-sx:x+sx), if isNorm(y)&&isNorm(x)
   *                averange in strings or colums, if isNorm(y)||isNorm(x)
   *                simple max(y)(x), , if (!isNorm(y))&&(!isNorm(x))
   */
  def localEX(mat: M, sy: Int, sx: Int): M = {
    logger.info(s"local meaning on matrix calculation started with sx=${sx} and sy=${sy}")
    val m = mat.length; val n = mat(0).length
    // fast sum in strings: O(h*w) operations
    val sumStr = createM(m, n)
    val dxs = -sx to sx
    for (y <- 0 until m) {
      sumStr(y)(sx) = dxs.map { dx => mat(y)(sx + dx) }.sum
      for (x <- sx + 1 until n - sx)
        sumStr(y)(x) = sumStr(y)(x - 1) + mat(y)(x + sx) - mat(y)(x - sx - 1)
    }
    // fast sum in columns of sum of string: O(h*w) operations
    val sumCol = createM(m, n)
    val dys = -sy to sy
    for (x <- 0 until n) {
      sumCol(sy)(x) = dys.map { dy => sumStr(sy + dy)(x) }.sum
      for (y <- sy + 1 until m - sy)
        sumCol(y)(x) = sumCol(y - 1)(x) + sumStr(y + sy)(x) - sumStr(y - sy - 1)(x)
    }

    // norming
    val S: T = (2 * sx + 1) * (2 * sy + 1)
    val SX: T = (2 * sx + 1); val SY: T = (2 * sy + 1);
    val mx = createM(m, n)
    // (sy:m-sy-1)(xs:n-sx-1)
    for (y <- sy until m - sy)
      for (x <- sx until n - sx)
        mx(y)(x) = sumCol(y)(x) / S
    // for x: (sy:m-sy)(0:xs-1) and (sy:m-sy)(n-xs+1:n-xs-1)
    for (y <- sy until m - sy) {
      for (x <- 0 until sx)
        mx(y)(x) = sumCol(y)(x) / SY
      for (x <- n - sx + 1 until n)
        mx(y)(x) = sumCol(y)(x) / SY
    }
    // for y: ...
    for (x <- sx until n - sx) {
      for (y <- 0 until sy)
        mx(y)(x) = sumCol(y)(x) / SX
      for (y <- m - sy + 1 until m)
        mx(y)(x) = sumCol(y)(x) / SX
    }
    // angles
    for (x <- 0 until sx) {
      for (y <- 0 until sy)
        mx(y)(x) = mat(y)(x)
      for (y <- m - sy + 1 until m)
        mx(y)(x) = mat(y)(x)
    }
    for (y <- 0 until sy) {
      for (x <- 0 until sx)
        mx(y)(x) = mat(y)(x)
      for (x <- n - sx + 1 until n)
        mx(y)(x) = mat(y)(x)
    }
    mx
  }

  /** @param sy -- half size on hight
   *  @param sx -- half size on weight
   *  def: isNorm(z) = (z>=sz)&&(z<maxZ-sz)
   *  mat(y)(x) => averange in window mat(y-sy:y+sy)(x-sx:x+sx), if isNorm(y)&&isNorm(x)
   *                averange in strings or colums, if isNorm(y)||isNorm(x)
   *                simple max(y)(x), , if (!isNorm(y))&&(!isNorm(x))
   */
  def localEX(mat: MInt, sy: Int, sx: Int): MInt = {
    logger.info(s"local meaning on matrix calculation started with sx=${sx} and sy=${sy}")
    val m = mat.length; val n = mat(0).length
    // fast sum in strings: O(h*w) operations
    val sumStr = createMInt(m, n)
    val dxs = -sx to sx
    for (y <- 0 until m) {
      sumStr(y)(sx) = dxs.map { dx => mat(y)(sx + dx) }.sum
      for (x <- sx + 1 until n - sx)
        sumStr(y)(x) = sumStr(y)(x - 1) + mat(y)(x + sx) - mat(y)(x - sx - 1)
    }
    // fast sum in columns of sum of string: O(h*w) operations
    val sumCol = createMInt(m, n)
    val dys = -sy to sy
    for (x <- 0 until n) {
      sumCol(sy)(x) = dys.map { dy => sumStr(sy + dy)(x) }.sum
      for (y <- sy + 1 until m - sy)
        sumCol(y)(x) = sumCol(y - 1)(x) + sumStr(y + sy)(x) - sumStr(y - sy - 1)(x)
    }

    // norming
    val S: Int = (2 * sx + 1) * (2 * sy + 1)
    val SX: Int = (2 * sx + 1); val SY: Int = (2 * sy + 1);
    val mx = createMInt(m, n)
    // (sy:m-sy-1)(xs:n-sx-1)
    for (y <- sy until m - sy)
      for (x <- sx until n - sx)
        mx(y)(x) = sumCol(y)(x) / S
    // for x: (sy:m-sy)(0:xs-1) and (sy:m-sy)(n-xs+1:n-xs-1)
    for (y <- sy until m - sy) {
      for (x <- 0 until sx)
        mx(y)(x) = sumCol(y)(x) / SY
      for (x <- n - sx + 1 until n)
        mx(y)(x) = sumCol(y)(x) / SY
    }
    // for y: ...
    for (x <- sx until n - sx) {
      for (y <- 0 until sy)
        mx(y)(x) = sumCol(y)(x) / SX
      for (y <- m - sy + 1 until m)
        mx(y)(x) = sumCol(y)(x) / SX
    }
    // angles
    for (x <- 0 until sx) {
      for (y <- 0 until sy)
        mx(y)(x) = mat(y)(x)
      for (y <- m - sy + 1 until m)
        mx(y)(x) = mat(y)(x)
    }
    for (y <- 0 until sy) {
      for (x <- 0 until sx)
        mx(y)(x) = mat(y)(x)
      for (x <- n - sx + 1 until n)
        mx(y)(x) = mat(y)(x)
    }
    mx
  }

  /** local averange for matrix from square of each element of `mat`
   *  @param mat: mat => localEX(mat.^2), where .^2 is operation of square for each element
   *  @see @{link #localEX}
   */
  def localEX2(mat: M, sy: Int, sx: Int): M = {
    val mat2 = mat.map { str => str.map { x => x * x } }
    localEX(mat2, sy, sx)
  }

  /** local averange for matrix from square of each element of `mat`
   *  @param mat: mat => localEX(mat.^2), where .^2 is operation of square for each element
   *  @see @{link #localEX}
   */
  def localEX2(mat: MInt, sy: Int, sx: Int): MInt = {
    val mat2: MInt = mat.map { str => str.map { x => x * x } }
    localEX(mat2, sy, sx)
  }

  /** local dispersion of matrix `mat`
   *  @param mat: mat => localEX(mat.^2) - localEX(mat).^2, where .^2 is operation of square for each element
   *  @see @{link #localEX} and @{link #localEX2}
   */
  def localDisp(mat: M, sy: Int, sx: Int): M = {
    logger.info(s"local dispersion on matrix calculation started with sx=${sx} and sy=${sy}")
    val ex1 = localEX(mat, sy, sx)
    val ex2 = localEX2(mat, sy, sx)
    val m = mat.length; val n = mat(0).length
    val res = createM(m, n)
    for (i <- 0 until m; j <- 0 until n)
      res(i)(j) = ex2(i)(j) - sqr(ex1(i)(j))
    res
  }

  /** local dispersion of matrix `mat`
   *  @param mat: mat => localEX(mat.^2) - localEX(mat).^2, where .^2 is operation of square for each element
   *  @see @{link #localEX} and @{link #localEX2}
   */
  def localDisp(mat: MInt, sy: Int, sx: Int): MInt = {
    logger.info(s"local dispersion on matrix calculation started with sx=${sx} and sy=${sy}")
    val ex: MInt = localEX(mat, sy, sx)
    val ex2: MInt = localEX2(mat, sy, sx)
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (i <- 0 until m; j <- 0 until n)
      res(i)(j) = ex2(i)(j) - sqr(ex(i)(j))
    res
  }

  /** @see #other.MathToolKit.correlation */
  def correlation(img1: BI, img2: BI, colorID: Int): T = {
    logger.info(s"image correlation calculation started with colorId=${colorId}")
    val mat1 = mapIT(image.Operation.getColorsComponents(img1, colorID), (x: Int) => x.toDouble)
    val mat2 = mapIT(image.Operation.getColorsComponents(img2, colorID), (x: Int) => x.toDouble)
    val cor = postprocessing.Statistic.correlation(mat1, mat2)
    cor
  }

  /** @see #other.MathToolKit.disp */
  def disp(img: BI, colorID: Int): T = {
    logger.info(s"image dispersion calculation started with colorId=${colorId}")
    val mat = mapIT(image.Operation.getColorsComponents(img, colorID), (x: Int) => x.toDouble)
    val aver = postprocessing.Statistic.aver(mat)
    postprocessing.Statistic.disp(mat, aver)
  }
}
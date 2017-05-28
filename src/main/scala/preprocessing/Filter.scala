package preprocessing

import main.Basic._
import math._

object Filter {
  val MAX = 255
  val MIN = 0

  def histogramFilterMin(img: BI): BI = {
    val ind = image.Analys.minHistogram(img)
    println(ind)
    image.Operation.toBinary(img, ind)
  }

  def histogramFilterMax(img: BI): BI = {
    val gi = image.Analys.histogram(img)
    val maxInd = 5 + gi.indexOf((5 until 250).map { gi(_) }.max)
    val ind1 = maxInd - 1
    val ind2 = maxInd + 1
    println(ind1, ind2)
    val mat = imgToMInt(img)
    for (i <- 0 until mat.length; j <- 0 until mat(0).length)
      mat(i)(j) =
        if (mat(i)(j) < ind1) MIN
        else if (mat(i)(j) > ind2) MAX
        else MAX * mat(i)(j) / (ind2 - ind1)
    intMatToImg(mat)
  }

  def gaus(mat: MInt, r: Int, sigma: T): M = {
    val L = sqrt(1.0 / (2 * Pi * sqr(sigma)))
    val invSigma2 = 1.0 / (2 * sqr(sigma))
    val m = mat.length; val n = mat(0).length
    // L*L * exp(-invSigma2 * (x * x + y * y))
    val ls = (0 to r).map { x => L * exp(-invSigma2 * (x * x)) }
    val strSum = createM(m, n)
    for (y <- r until m - r; x <- r until n - r) {
      strSum(y)(x) = mat(y)(x) * ls(0)
      for (p <- 1 to r)
        strSum(y)(x) += (mat(y)(x + p) + mat(y)(x - p)) * ls(p)
    }
    val colSum = createM(m, n)
    for (y <- r until m - r; x <- r until n - r) {
      colSum(y)(x) = strSum(y)(x) * ls(0)
      for (p <- 1 to r)
        colSum(y)(x) += (strSum(y + p)(x) + strSum(y - p)(x)) * ls(p)
    }
    colSum
  }

  def MSR(img: BI, r: Int): BI = {
    val matG = image.Input.getColorsComponents(img, 2)
    image.Output.visible(intMatToImg(matG), "inv green")
    val matB = image.Input.getColorsComponents(img, 1)
    val matR = image.Input.getColorsComponents(img, 3)
    val m = matG.length; val n = matG(0).length
    val sumMat = createM(m, n)
    for (i <- 0 until m; j <- 0 until n)
      sumMat(i)(j) = matG(i)(j) + matB(i)(j) + matR(i)(j)
    def getMSR(id: Int): M = {
      val (mat, sigma_i) = id match {
        case 1 => (matB, 15)
        case 2 => (matG, 80)
        case 3 => (matR, 250)
      }
      // TODO: Misha: maybe made as parallel?
      val (_G, b, alpha, beta) = (192, -30, 125, 46)
      val res = createM(m, n)
      val ga = gaus(mat, r, sigma_i)
      for (i <- r until m - r; j <- 0 until n - r) {
        if (mat(i)(j) != 0 && mat(i)(j) != MAX && ga(i)(j) != 0) {
          val C_ij = beta * (log(alpha * (mat(i)(j))) - log(sumMat(i)(j)))
          res(i)(j) = _G * (C_ij * (log(mat(i)(j)) - log(ga(i)(j))) + b)
        }
      }
      preprocessing.Filter.adaptiveContrast(res)
      res
    }
    val resG = mapTI(getMSR(2), toInt(_))
    //  val resB = map(getMSR(1), toInt)
    //  val resR = map(getMSR(3), toInt)
    image.Operation.createImage(resG, img.getType)
  }

  def inverse(mat: MInt) {
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = MAX - mat(i)(j)
  }
  def inverse(mat: M) {
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = MAX - mat(i)(j)
  }
  def inverse(img: BI): BI = {
    val mat = imgToMInt(img)
    inverse(mat)
    intMatToImg(mat)
  }
  def fullInverse(img: BI): BI = {
    val mats = (1 to 3).map { image.Input.getColorsComponents(img, _) }
    mats.map { inverse(_) }
    image.Operation.createImage((mats(0), mats(1), mats(2)), img.getType)
  }

  /** x => MAX*(x-l)/(h-l) */
  def constrast(mat: MInt, h: Int, l: Int) {
    val m = mat.length; val n = mat(0).length
    def toNew(x: Int) =
      if (x > h) MAX
      else if (x < l) MIN
      else MAX * (x - l) / (h - l)
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = toNew(mat(i)(j))
  }

  /** x => 255*(x-l)/(h-l) */
  def constrast(img: BI, h: Int = 200, l: Int = 50) {
    val m = img.getHeight; val n = img.getWidth
    def toNew(x: Int) =
      if (x > h) 255
      else if (x < l) 0
      else 255 * (x - l) / (h - l)
    for (y <- 0 until m; x <- 0 until n) {
      val rgb = img.getRGB(x, y)
      val b = rgb & 255
      val g = (rgb >> 8) & 255
      val r = (rgb >> 16) & 255
      val newRgb = toNew(b) + (toNew(g) << 8) + (toNew(r) << 16)
      img.setRGB(x, y, newRgb)
    }
  }

  def adaptiveContrast(mat: MInt) {
    val (mn, mx) = main.Statistic.minMax(mat)
    val m = mat.length; val n = mat(0).length
    def toNew(x: Int) =
      if (x > mx) MAX
      else if (x < mn) MIN
      else MAX * (x - mn) / (mx - mn)
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = toNew(mat(i)(j))
  }

  def adaptiveContrast(mat: M) {
    val (mn, mx) = main.Statistic.minMax(mat)
    val m = mat.length; val n = mat(0).length
    def toNew(x: T): T =
      if (x > mx) MAX
      else if (x < mn) MIN
      else MAX * (x - mn) / (mx - mn)
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = toNew(mat(i)(j))
  }
}
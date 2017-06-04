package preprocessing

import main.Basic._
import math._

object Filter {
  val MAX = 255
  val MIN = 0

  def smallFilter(img: BI, amountBorder: Int): BI = {
    val mat = imgToMInt(img)
    val m = mat.length; val n = mat(0).length
    def isWhite(x: Int): Boolean = (x == 255)
    var S = 0
    def analysSmallFilter(i: Int, j: Int, delete: B = false) {
      var centralX = j
      var dy = 0
      while (i + dy >= 0 && isWhite(mat(i + dy)(centralX))) {
        centralX = getNewCentral(dy, centralX)
        dy -= 1
      }
      dy = 1
      while (i + dy < m && isWhite(mat(i + dy)(centralX))) {
        centralX = getNewCentral(dy, centralX)
        dy += 1
      }

      def getNewCentral(dy: Int, oldCenter: Int): Int = {
        val leftBordX = {
          var dx = 1
          while (j + dx >= 0 && isWhite(mat(i + dy)(oldCenter + dx))) {
            if (delete) mat(i + dy)(oldCenter + dx) = 0
            dx -= 1
          }
          j + dx + 1
        }
        val rightBordX = {
          var dx = 1
          while (j + dx < n && isWhite(mat(i + dy)(oldCenter + dx))) {
            if (delete) mat(i + dy)(oldCenter + dx) = 0
            dx += 1
          }
          j + dx - 1
        }
        S += rightBordX - leftBordX
        val centralX = (leftBordX + rightBordX) / 2
        centralX
      }
      if (delete)
        if (S < amountBorder)
          analysSmallFilter(i, j, true)
    }
    for (i <- 0 until m; j <- 0 until n)
      if (isWhite(mat(i)(j)))
        analysSmallFilter(i, j)
    intMatToImg(mat)
  }

  def histogramFilterMin(img: BI): BI = {
    val ind = postprocessing.Histogram.minHistogram(img)
    println(ind)
    image.Operation.toBinary(img, ind)
  }

  def histogramFilterMax(img: BI): BI = {
    val gi = postprocessing.Histogram.histogram(img)
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
    val RGB = image.Operation.getColorsComponents(img)
    val R = RGB._1; val G = RGB._2; val B = RGB._3
    val m = R.length; val n = R(0).length
    val sumMat = createM(m, n)
    for (i <- 0 until m; j <- 0 until n)
      sumMat(i)(j) = R(i)(j) + G(i)(j) + B(i)(j)

    def getMSR(id: Int): M = {
      val (mat, sigma_i) = id match {
        case 1 => (B, 15)
        case 2 => (G, 80)
        case 3 => (R, 250)
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
    image.Operation.createTiffImage(resG)
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
    val mats = (1 to 3).map { image.Operation.getColorsComponents(img, _) }
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
    val (mn, mx) = postprocessing.Statistic.minMax(mat)
    val m = mat.length; val n = mat(0).length
    def toNew(x: Int) =
      if (x > mx) MAX
      else if (x < mn) MIN
      else MAX * (x - mn) / (mx - mn)
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = toNew(mat(i)(j))
  }

  def adaptiveContrast(mat: M) {
    val (mn, mx) = postprocessing.Statistic.minMax(mat)
    val m = mat.length; val n = mat(0).length
    def toNew(x: T): T =
      if (x > mx) MAX
      else if (x < mn) MIN
      else MAX * (x - mn) / (mx - mn)
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = toNew(mat(i)(j))
  }
}
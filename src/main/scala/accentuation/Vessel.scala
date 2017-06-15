package accentuation

import math._
import other.Types._
import image._

object Vessel {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  val stepTheta = 10
  val border = 128

  def accent(img: BI, r: Int, s: Int, extr: String): (BI, BI, BI) = {
    logger.info(s"accentuation vessel on image started with radius=${r} and s=${s}, ${extr}")

    val (vesselM, directMInt, thinVesselMat): (MInt, MInt, MInt) =
      accent(imgToMInt(img), r, s, extr)

    val vesImg = image.Operation.createTiffImage(vesselM)
    val directImg = image.Operation.createTiffImage(directMInt)
    val thinVesselImg = image.Operation.createTiffImage(thinVesselMat)
    (vesImg, directImg, thinVesselImg)
  }

  def accent(mat: MInt, d1: Int, s: Int, extr: String): (MInt, MInt, MInt) = {
    logger.info(s"accentuation vessel on matix started with radius ${d1} and s=${s}, ${extr}")

    val matUpd = preprocessing.Illumination.illumination(mat, s)

    val m = mat.length; val n = mat(0).length
    val (lr1, rr1) =
      if (d1 % 2 == 0) (d1 / 2, d1 / 2)
      else (d1 / 2, d1 / 2)

    val (lr2, rr2) = (lr1 + 2, rr1 + 2)
    val antiL = -lr1
    val antiR = -rr1
    // core.sum = 0
    val core = (-lr2 to rr2).map {
      i =>
        if (i == 0) 0 else if (i >= -lr1 && i <= rr1) 2
        else if (i > 0) antiR
        else antiL
    }

    def locMask(i: Int, j: Int, theta: Int): Int = {
      var sum: Int = 0
      for (r <- -lr2 to rr2) {
        val dx: Int = deltaX(r, theta)
        val dy: Int = deltaY(r, theta)
        sum += mat(i + dy)(j + dx) * core(r + lr2)
      }
      sum
    }

    def getTransAndDir(i: Int, j: Int): (Int, Int) =
      extr match {
        case "MAX"    => maxSearch(i, j, locMask(i, j, _))
        case "MIN"    => minSearch(i, j, locMask(i, j, _))
        case "MAXMIN" => maxMinSearch(i, j, locMask(i, j, _), mat(i)(j))
        case "MINMAX" => minMaxSearch(i, j, locMask(i, j, _), mat(i)(j))
      }

    val resTranform: MInt = createMInt(m, n)
    val trueDir: MInt = createMInt(m, n)
    (lr2 until m - rr2).par.map {
      i =>
        for (j <- lr2 until n - rr2) {
          val transAndDir = getTransAndDir(i, j)
          resTranform(i)(j) = if (transAndDir._1 < 128) 0 else 255
          trueDir(i)(j) = transAndDir._2
        }
    }

    val res = resTranform.map { _.map { _.toInt } }
    postprocessing.Mediate.mediate(trueDir, res)

    (resTranform, trueDir, res)
  }

  def maxSearch(i: Int, j: Int, locMask: Int => Int): (Int, Int) = {
    var mx = Int.MinValue
    var dir = 0
    for (theta <- 0 until 180 by stepTheta) {
      val lm = locMask(theta)
      if (mx < lm) {
        mx = lm
        dir = theta
      }
    }
    (mx, dir)
  }

  def minSearch(i: Int, j: Int, locMask: Int => Int): (Int, Int) = {
    var mn = Int.MaxValue
    var dir = 0
    for (theta <- 0 until 180 by stepTheta) {
      val lm = locMask(theta)
      if (mn > lm) {
        mn = lm
        dir = theta
      }
    }
    (mn, dir)
  }

  def minMaxSearch(i: Int, j: Int, locMask: Int => Int, matIJ: Int) =
    if (matIJ < border) minSearch(i, j, locMask)
    else maxSearch(i, j, locMask)

  def maxMinSearch(i: Int, j: Int, locMask: Int => Int, matIJ: Int) =
    if (matIJ < border) maxSearch(i, j, locMask)
    else minSearch(i, j, locMask)
}
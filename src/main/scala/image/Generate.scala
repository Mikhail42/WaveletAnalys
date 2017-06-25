package image

import java.awt._
import java.awt.image._

import math._
import other.Types._

object Generate {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def generateImageMat: M = {
    // B is amplitude
    // C is frequency
    val B: T = 5; val C: T = 10
    val size = 100
    logger.debug(s"generate image with ampiltude in ${B} pixels and frequence in ${C} pixels, size=${size}x${size}")

    val picture: M = Array.fill[T](size, size)(255)
    for (y <- 0 until size; x <- 0 until size)
      for (A <- 30 to 190 by 30)
        picture(y)(x) =
          if (abs(y - A - B * sin(x / C)) < 6) 255 - A
          else min(picture(y)(x), 255)
    picture
  }

  def generateRectCol(outName: String) {
    val m = 255; val n = 20;
    logger.debug(s"generate Color Rectangle Image")
    val mat = Array.ofDim[Int](m, n)
    for (y <- 0 until m) {
      val eignte = 45 * 255 / 180 // = 66
      val col = if (y % eignte != 0) y else 0
      for (x <- 0 until n)
        mat(y)(x) = col
    }
    Output.saveImage(mat, outName)
  }

  def generateImageMat(amplit: T, freq: T, w: Int, h: Int): BI = {
    logger.debug(s"generate image with apmitude=${amplit}, frequence=${freq}, size=${w}x${h}")

    val picture: M = Array.fill[T](h, w)(255)
    for (y <- 0 until h; x <- 0 until w)
      for (A <- 30 to 190 by 30)
        picture(y)(x) =
          if (abs(y - A - amplit * sin(x / freq)) < 6) 255 - A
          else min(picture(y)(x), 255)
    val gRes: MInt = Convert.toColorMInt(picture)
    Convert.createTiffImage(gRes)
  }
}
package postprocessing

import math._
import java.awt.image._

import other.Types._
import other.Constants._

import image.Convert._

object Compare {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** compare binary images
   *  @param img1 -- my image: type == 10, all values is -1 or 0
   *  @param img2 -- ideal image: type == 10, all values is -1 or 0
   *  @return pair (errorWhite/white, errorBlack/black)
   */
  def compareBinaryImages(img1: BI, img2: BI): (T, T) = {
    logger.info("compareBinaryImages started")

    val pixs1 = getPixels(img1)
    val pixs2 = getPixels(img2)
    compare(pixs1, pixs2, 0, 255.toByte)
  }

  def compareBinaryMatrix(mat1: MInt, mat2: MInt, blackColor: Int = 0, whiteColor: Int = 255) = {
    val m = mat1.length; val n = mat1(0).length;
    if (m * n != mat2.length * mat2(0).length)
      throw new exceptions.ImageInfoException(
        s"Length of 1th images is ${m * n}, but length of 2th images is ${mat2.length * mat2(0).length}!")

    var white = 0
    var errorWhite = 0
    var black = 0
    var errorBlack = 0

    for (i <- 0 until m; j <- 0 until n) {
      if (mat2(i)(j) == whiteColor) {
        white += 1
        if (mat1(i)(j) != whiteColor)
          errorWhite += 1
      } else if (mat2(i)(j) == blackColor) {
        black += 1
        if (mat1(i)(j) != blackColor)
          errorBlack += 1
      }
      // else exception, see down
    }

    if (white + black != m * n)
      throw new exceptions.ImageInfoException("Images to compare must be binary")

    (errorWhite.toDouble / white, errorBlack.toDouble / black)
  }

  def compare(pixs1: Array[Byte], pixs2: Array[Byte], blackColor: Byte = 0.toByte, whiteColor: Byte = -1.toByte) = {
    if (pixs1.length != pixs2.length)
      throw new exceptions.ImageInfoException(s"Length of 1th images is ${pixs1.length}, but length of 2th images is ${pixs2.length}!")

    val n = pixs1.length

    var white = 0
    var errorWhite = 0
    var black = 0
    var errorBlack = 0

    for (i <- 0 until n) {
      if (pixs2(i) == whiteColor) {
        white += 1
        if (pixs1(i) != whiteColor)
          errorWhite += 1
      } else if (pixs2(i) == blackColor) {
        black += 1
        if (pixs1(i) != blackColor)
          errorBlack += 1
      }
      // else exception, see down
    }

    if (white + black != n)
      throw new exceptions.ImageInfoException("Images to compare must be binary")

    (errorWhite.toDouble / white, errorBlack.toDouble / black)
  }
}
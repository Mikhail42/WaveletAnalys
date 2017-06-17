package postprocessing

import math._
import java.awt.image._

import other.Types._
import other.Constants._

import image.Operation._

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
    if (pixs1.length != pixs2.length)
      throw new exceptions.ImageInfoException("Probably, Image Format Exception or Size Image Exception")

    val n = pixs1.length
    var white = 0
    var errorWhite = 0
    var black = 0
    var errorBlack = 0
    val m1: Byte = -1 // probably, it is white
    val m0: Byte = 0 // probably, it is black

    for (i <- 0 until n) {
      if (pixs2(i) == m1) {
        white += 1
        if (pixs1(i) != m1)
          errorWhite += 1
      } else if (pixs2(i) == m0) {
        black += 1
        if (pixs1(i) != m0)
          errorBlack += 1
      }
      // else exception, see down
    }

    if (white + black != n)
      throw new exceptions.ImageInfoException("Images to compare must be binary")

    (errorWhite.toDouble / white, errorBlack.toDouble / black)
  }
}
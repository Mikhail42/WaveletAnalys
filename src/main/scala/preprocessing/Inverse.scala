package preprocessing

import other.Types._
import math._

object Inverse {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  val MAX = 255
  val MIN = 0

  def inverse(mat: MInt) {
    logger.trace(s"inverse matrix started")
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = MAX - mat(i)(j)
  }
  def inverse(mat: M) {
    logger.trace(s"inverse matrix started")
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      mat(i)(j) = MAX - mat(i)(j)
  }
  def inverse(img: BI): BI = {
    logger.trace(s"inverse image started")
    val mat = imgToMInt(img)
    inverse(mat)
    intMatToImg(mat)
  }
  def fullInverse(img: BI): BI = {
    logger.trace(s"full inverse image started")
    val mats = image.Operation.getColorsComponents(img)
    inverse(mats._1); inverse(mats._2); inverse(mats._3)
    image.Operation.createImage(mats, img.getType)
  }

}
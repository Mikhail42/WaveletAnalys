package postprocessing

import math._
import other.Types._
import other.Constants._

object Contrast {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** @return 255.0*(x-mn)/(mx-mn)) or bordered value */
  def contrast(mat: M, mn: T, mx: T): M = {
    logger.debug(s"contrast of matrix with max=${mx} and min=${mn}")

    val c = 255.0 / (mx - mn)
    mapTT(mat,
      (x: T) =>
        if (x < mn) 0.0
        else if (x > mx) 255.0
        else (x - mn) * c)
  }

  /** @return 255*(x-mn)/(mx-mn)) or bordered value */
  def contrast(mat: MInt, mn: Int, mx: Int): MInt = {
    logger.debug(s"contrast of matrix with max=${mx} and min=${mn}")
    mapII(mat,
      (x: Int) =>
        if (x < mn) 0
        else if (x > mx) 255
        else 255 * (x - mn) / (mx - mn))
  }
}
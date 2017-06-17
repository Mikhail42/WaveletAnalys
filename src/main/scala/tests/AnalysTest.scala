package tests

import math._

import Base._
import other.Types._
import image._

object AnalysTest {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** vessels accentuation test (0 until 180 by 10 degree) */
  def vesselSegmentTest {
    val fileName = dir + forVessel
    logger.info(s"vessel assentuation test started for ${fileName}")
    val img = Input.uploadImage(fileName)
    val mat = image.Operation.getColorsComponents(img, 2)
    for (d1 <- 14 to 14; s <- 9 to 9) {
      val ves = new accentuation.Vessel(mat, d1, s)
      val res = ves.accent
      //val res = accentuation.Vessel.accent(mat, d1, s, "MAX")
      Output.visible(res._1, s"output image, diameter=${d1}, s=${s}")
    }
  }

  /** disk accentuation test */
  def diskTest {
    val fileName = dir + forDisk
    logger.info(s"disc assentuation test started for ${fileName}")
    val img = image.Input.uploadImage(fileName)
    for (r <- 2 to 25 by 2) {
      val res = accentuation.Disk.emphasis(img, r)
      image.Output.visible(res, "Disc Test" + r)
    }
  }
}
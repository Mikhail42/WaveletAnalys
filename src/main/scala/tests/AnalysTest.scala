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
    val img = Input.uploadSubimage(fileName)
    val mat = image.Operation.getColorsComponents(img, 2)
    for (r <- 13 to 14; s <- 2 to 2) {
      val res = accentuation.Vessel.accent(mat, r, s, "MAX")
      Output.visible(res._1, s"out ${r} ${s}")
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
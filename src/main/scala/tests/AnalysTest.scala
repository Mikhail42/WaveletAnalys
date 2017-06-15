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
    val img = Input.uploadSubimage(fileName)
    for (r <- 13 to 14; s <- 2 to 2) {
      val res = accentuation.Vessel.accent(img, r, s, "MAX")
      Output.visible(res._1, s"out ${r} ${s}")
    }
  }

  /** disk accentuation test */
  def diskTest {
    val fileName = dir + forDisk
    println(fileName)
    val img = image.Input.uploadImage(fileName)
    for (r <- 2 to 25 by 2) {
      val res = accentuation.Disk.emphasis(img, r)
      val res2 = preprocessing.Inverse.inverse(res)
      image.Output.visible(res2, "Disc Test" + r)
    }
  }
}
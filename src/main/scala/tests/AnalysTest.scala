package tests

import math._

import Base._
import other.Types._
import image._

object AnalysTest {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** vessels accentuation test (0 until 180 by 10 degree) */
  def vesselSegmentTest {
    val fileName1 = dir + forVessel
    val fileName2 = dir + forTiff
    logger.info(s"vessel assentuation test started for ${fileName1}")
    val img1 = Input.uploadSubimage(fileName1)

    val img2NotBinary = Input.uploadSubimage(fileName2)
    val mat1 = Convert.getColorsComponents(img1, 2)
    val mat2 = Convert.getColorsComponents(img2NotBinary, 2)
    //val mat1 = preprocessing.Illumination.illumination(mat1Old, 10)

    val s = 10; val d1 = 14
    for (d1 <- 2 to 20 by 2) {
      val vessel = new accentuation.Vessel(mat1, d1)
      val res = vessel.accentuation()

      val (erWhite, erBlack) = postprocessing.Compare.compareBinaryMatrix(res._1, mat2)

      def errorPercent(er: T): Int = (er * 100).round.toInt
      val erWhitePerc = errorPercent(erWhite); val erBlackPerc = errorPercent(erBlack)
      //if (erWhitePerc < 97 && erBlackPerc < 10) {
      logger.info(s"d1 = ${d1}, s=${s}, error white is ${erWhitePerc}%, error black is ${erBlackPerc}%")
      image.Output.visible(res._1, s"best ${d1}")
      //}
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
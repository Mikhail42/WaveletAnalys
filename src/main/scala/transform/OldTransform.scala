package transform

import math._
import other.Types._
import image.Input._
import image.Convert._
import image._

object OldTransform {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** universal def to wavelt's transform.
   *  @param transf -- some (wavelet) transform
   */
  def wavelet(img: BI, transf: (M, T) => (M, MInt), nameWavelet: String, a: T): (BI, BI, BI) = {
    logger.debug(s"${nameWavelet} transform of image with a=${a}")
    val mat: M = getColorsComponents(img, 2).map { _.map { _.toDouble } }
    val m = mat.length; val n = mat(0).length
    val (res, dir) = transf(mat, a)

    val resIntImg = res.map { _.map { _.toInt } }
    postprocessing.Mediate.mediate(dir, resIntImg)
    val thinyImg = Convert.createTiffImage(resIntImg)
    val dirImg = Convert.createTiffImage(dir)
    val resImg = Convert.createTiffImage(res)
    preprocessing.Filter.constrast(resImg, 140, 20)

    (resImg, dirImg, thinyImg)
  }

  def asVesselSpecTransform(img: BI, oldAsVes: wavelets.OldAsVessel): (BI, BI, BI) =
    wavelet(img, oldAsVes.specTransform, "asVesselSpec", a = 1)
}
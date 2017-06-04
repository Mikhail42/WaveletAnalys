package transform

import math._
import other.Basic._
import image.Input._
import image.Operation._
import image._

object OldTransform {
  /** universal def to wavelt's transform.
   *  @param transf -- some (wavelet) transform
   */
  def wavelet(img: BI, transf: (M, T) => (M, MInt), nameWavelet: String, a: T): (BI, BI, BI) = {
    val mat: M = getColorsComponents(img, 2).map { _.map { _.toDouble } }
    val m = mat.length; val n = mat(0).length
    val (res, dir) = transf(mat, a)

    val resIntImg = res.map { _.map { _.toInt } }
    postprocessing.Mediate.mediate(dir, resIntImg)
    val thinyImg = Operation.createTiffImage(resIntImg)
    val dirImg = Operation.createTiffImage(dir)
    val resImg = Operation.createTiffImage(res)
    preprocessing.Filter.constrast(resImg, 140, 20)

    (resImg, dirImg, thinyImg)
  }

  def asVesselSpecTransform(img: BI, oldAsVes: wavelets.OldAsVessel): (BI, BI, BI) =
    wavelet(img, oldAsVes.specTransform, "asVesselSpec", a = 1)
}
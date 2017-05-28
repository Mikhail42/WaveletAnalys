package transform

import math._
import main.Basic._
import image.Input._
import image._
    
object OldTransform {
  /** universal def to wavelt's transform. 
   *  @param transf -- some (wavelet) transform */
  def wavelet(img: BI, transf: (M, T) => (M, MInt), nameWavelet: String, a: T): (BI, BI, BI) = {
    val mat: M = getColorsComponents(img, 2).map{_.map{_.toDouble}}
    val m = mat.length; val n = mat(0).length
    val (res, dir) = transf(mat, a)
    
    val resIntImg = res.map{_.map{_.toInt}}
    Analys.mediate(dir, resIntImg)
    val thinyImg = Operation.toImage(resIntImg)
    
    val dirImg = Operation.toImage(dir)    
    
    val resImg = Operation.matrixToImage(res)
    preprocessing.Filter.constrast(resImg, 140, 20)
    
    (resImg, dirImg, thinyImg)
  }
  
  def asVesselSpecTransform(img: BI, oldAsVes: wavelets.OldAsVessel): (BI, BI, BI) =
    wavelet(img, oldAsVes.specTransform, "asVesselSpec", a = 1)
}
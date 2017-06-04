package transform

import java.awt.color._
import java.awt.image.ColorConvertOp
import java.awt.image.BufferedImage
import math._
import scala.collection.mutable.ArraySeq
import scala.collection.immutable.IndexedSeq
import other.Basic._
import image._

object DTransform  {
  
  def daubechies(
      mat: M,
      order: Int = 2, 
      transformID: String = "mat"): M = {
    val wavelet = new wavelets.Daubechies(order)
    val trans   = new transform.AncientEgyptianDecomposition(wavelet)
    val resMat: M = trans.forward2D(mat, transformID)
    resMat
  }
  
  def daubechiesForwardAndReverse(
      mat: M, 
      order: Int = 2, 
      transformID: String = "mat"): M = {
    val wavelet = new wavelets.Daubechies(order)
    val trans   = new transform.AncientEgyptianDecomposition(wavelet)
    val resMat: M = trans.forward2D(mat, transformID)
    val invresMat: M = trans.reverse2D(resMat, transformID)
    invresMat
  }
}
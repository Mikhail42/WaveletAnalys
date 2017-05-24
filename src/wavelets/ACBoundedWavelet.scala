package wavelets

import basic.Basic._
import math._
import basic.Integral

abstract class ACBoundedWavelet(d: T, a: T) extends ICWavelet {
  val dDivA: T = d/a
  def psi(x: T): T
  /** 2D wavelet-function */
  override def psi(x: T, y: T) : T = 
    if (abs(y) < dDivA) psi(x) else 0.0 
}
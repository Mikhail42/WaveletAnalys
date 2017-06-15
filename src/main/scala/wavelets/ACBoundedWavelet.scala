package wavelets

import other.Types._
import math._
import other.Integral

abstract class ACBoundedWavelet(d: T, a: T) extends ICWavelet {
  override val logger = com.typesafe.scalalogging.Logger(getClass)

  val dDivA: T = d / a
  def psi(x: T): T
  /** 2D wavelet-function */
  override def psi(x: T, y: T): T =
    if (abs(y) < dDivA) psi(x) else 0.0
}
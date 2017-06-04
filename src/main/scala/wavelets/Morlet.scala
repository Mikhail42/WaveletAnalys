package wavelets

import math._
import other.Types._

/** psi(x, y) = cos(x*k0x+y*k0y)*exp(-norm2(kx*x, y)) */
class Morlet(k0x: T = -1, k0y: T = 3, eps: T = 8) extends ICWavelet {
  override val wavename = "Morlet"
  val kx = 1.0 / sqrt(eps)
  /** Im-part of 2D wavelet-function */
  override def psi(x: T, y: T) : T = 
    cos(x*k0x+y*k0y)*exp(-norm2(kx*x, y))
}
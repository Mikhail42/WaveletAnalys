package wavelets
import basic.Basic._
import math._
import basic.Integral

class AsLongVessel(d: T = 4, a: T) extends ACBoundedWavelet(d, a) {
  override val wavename = "AsLongVessel"
  override def psi(x: T): T = (d-abs(x))*exp(-sqr(x))
}
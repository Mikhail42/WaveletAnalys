package wavelets
import main.Basic._
import math._
import main.Integral

class AsLongVessel(d: T = 4, a: T) extends ACBoundedWavelet(d, a) {
  override val wavename = "AsLongVessel"
  override def psi(x: T): T = (d-abs(x))*exp(-sqr(x))
}
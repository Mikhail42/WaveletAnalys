package wavelets
import other.Types._
import math._
import other.Integral

class AsVessel(d: T = 3, a: T) extends ACBoundedWavelet(d, a) {
  //override val logger = com.typesafe.scalalogging.Logger(getClass)

  override val wavename = "AsVessel"
  override def psi(x: T): T = exp(-0.5 * sqr(x))
}
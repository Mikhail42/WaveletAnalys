package wavelets

import math._
import other.Types._

class Gabor(sigma: T = 7) extends ICWavelet {
  override val wavename = "Gabor"
  val sigma2 = sigma*sigma
  override def psi(x: T, y: T): T = 
    exp(-norm2(x, y)/(2*sigma2))*cos(sigma*x)
}
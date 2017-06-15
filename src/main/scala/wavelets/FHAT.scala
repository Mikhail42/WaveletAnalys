package wavelets

import math._
import other.Types._

class FHAT(d: T = 3, a: T) extends ACBoundedWavelet(d, a) {
  //override val logger = com.typesafe.scalalogging.Logger(getClass)

  override val wavename = "FHAT"
  override def psi(x: T): T =
    if (abs(x) <= a) 1
    else if (abs(x) <= 2 * a + 1) -(2 * a + 1) / (2 * a + 2)
    else 0
}
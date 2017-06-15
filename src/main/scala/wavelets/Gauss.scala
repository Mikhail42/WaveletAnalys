package wavelets

import math._
import other.Types._

class Gauss extends ICWavelet {
  override val logger = com.typesafe.scalalogging.Logger(getClass)

  override val wavename = "Gauss"
  /** wavelet for blood vessel  */
  override def psi(x: T, y: T): T =
    exp(-0.5 * norm2(x, y))
}
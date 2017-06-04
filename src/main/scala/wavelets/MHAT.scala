package wavelets

import math._
import other.Basic._

class MHAT(d: T = 2, a: T) extends ACBoundedWavelet(d, a) {
  override val wavename = "MHAT"
  val norm = 2.0 / sqrt( 3.0*sqrt(Pi) )
  override def psi(x: T): T = {
    val l1 = (x-0.5);  val l2 = (x+0.5)
    val s1 = pow4(l1); val s2 = pow4(l2)
    norm*(l2*exp(-s2*0.25) - l1*exp(-0.25*s1))
  }
}
package postprocessing

import math._
import other.Types._

object Region {
  def findSimilar(ar: AInt, i0: Int, colorWidth: Int = 20): (Int, Int) = {
    val n = ar.length
    val c = ar(i0)
    val res = Array.ofDim[Boolean](n)
    val l = {
      val l0 = ar.indexWhere(p => (abs(p - c) > colorWidth), i0)
      if (l0 == -1) n else l0 - 1
    }
    val f = {
      val f0 = ar.lastIndexWhere(p => (abs(p - c) > colorWidth), i0)
      if (f0 == -1) 0 else f0 + 1
    }
    (f, l)
  }

}
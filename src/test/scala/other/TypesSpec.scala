package other

import org.scalatest.FlatSpec

import math._
import other.Types._
import other.Constants._

class TypesSpec extends FlatSpec {
  "deltaX(1, 30)" should "cos(30 degree).round" in {
    assert(deltaX(1, 30) == cos(30 * Pi / 180).round)
  }
  "deltaY(1, 30)" should "sin(30 degree).round" in {
    assert(deltaY(1, 30) == sin(30 * Pi / 180).round)
  }

  "toColorInt(-5)" should "0" in {
    assert(toColorInt(-5) == 0)
  }
  "toColorInt(265)" should "255" in {
    assert(toColorInt(-5) == 0)
  }

  "norm_2^2(3, 4)" should "25" in {
    assert(norm2(3, 4) == 3 * 3 + 4 * 4)
  }

  "log2(4)" should "2" in {
    assert(log2(4) == 2)
  }
  "log2(1)" should "0" in {
    assert(log2(1) == 0)
  }
  "log2(2)" should "1" in {
    assert(log2(1) == 0)
  }
  "log2(7)" should "2" in {
    assert(log2(1) == 0)
  }

  "exist k in N_0 : 2^k = 7" should "false" in {
    assert(isBinary(7) == false)
  }
  "exist k in N_0 : 2^k = 8" should "true" in {
    assert(isBinary(8) == true)
  }
}
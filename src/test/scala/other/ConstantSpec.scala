package other

import org.scalatest.FlatSpec

import math._
import other.Basic._
import other.Constants._

class ConstantSpec extends FlatSpec {
  "sin(30)" should "0.5" in {
    assertEquals(other.Constants.sins(30), 0.5, 1e-6)
  }
  "cos(60)" should "0.5" in {
    assertEquals(other.Constants.coss(60), 0.5, 1e-6)
  }
  "cos(90)" should "0" in {
    assertEquals(other.Constants.coss(90), 0, 1e-6)
  }
}
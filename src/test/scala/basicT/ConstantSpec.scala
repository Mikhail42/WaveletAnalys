package mainT

import org.scalatest.FlatSpec

import math._
import main.Basic._
import main.Constants._

class ConstantSpec extends FlatSpec {
  "sin(30)" should "0.5" in {
    assertEquals(main.Constants.sins(30), 0.5, 1e-6)
  }
  "cos(60)" should "0.5" in {
    assertEquals(main.Constants.coss(60), 0.5, 1e-6)
  }
  "cos(90)" should "0" in {
    assertEquals(main.Constants.coss(90), 0, 1e-6)
  }
}
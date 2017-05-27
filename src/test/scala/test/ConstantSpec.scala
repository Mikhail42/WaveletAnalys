package tests

import org.scalatest.FlatSpec

import math._ 
import basic.Basic._
import basic.Constants._

class ConstantSpec extends FlatSpec  {
  "sin(30)" should "0.5" in {
    assertEquals( basic.Constants.sins(30), 0.5, 1e-6)
  }
  "cos(60)" should "0.5" in {
    assertEquals( basic.Constants.coss(60), 0.5, 1e-6)
  }
  "cos(90)" should "0" in {
    assertEquals( basic.Constants.coss(90), 0, 1e-6)
  }
}
package basic

import org.scalatest.FlatSpec

import math._ 
import main.Basic._
import main.Integral._

class IntegralSpec extends FlatSpec  {
  
  "Integral from 0 to 2.1 by 0.1" should "has 21 point" in {
    assert(getN(0, 2.1, 0.1) == 21)
  }
  "Integral from 0 to 2.0 by 0.1" should "has 21 point" in {
    assert(getN(0, 2.0, 0.1) == 21)
  }
  "int_0^1 x^2 dx " should "equals 1/3" in {
    assertEquals(simpson(x => x*x, 0, 1, 0.005), 1.0/3.0, 1e-2)
  }
  "int_0^Pi 1 dx " should "equals Pi" in {
    assertEquals(simpson(x => 1, 0, Pi, 0.01),  Pi, 1e-2)
  }
  "int_0^Pi sin(x) dx " should "equals 2" in {
    assertEquals(simpson(sin, 0, Pi, 0.01), 2, 1e-4)
  }
  "int_0^Pi int_0^Pi sin(x)sin(y) dx dy " should "equals 4" in {
    def fun(x: T, y: T) = sin(x) * sin(y)
    assertEquals(simpson2(fun, (0, 0), (Pi, Pi), 0.01),  4, 1e-4)
  }
  "int_0^1 int_0^1 1 dx dy " should "equals 1" in {
    assertEquals(simpson2((x, y) => 1, (0, 0), (1, 1), 0.005),  1, 1e-2)
  }  
}
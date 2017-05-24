package tests

import basic.Integral
import basic.Basic._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import math._ 

@RunWith(classOf[JUnitRunner])
object AllTest {
  def assertEquals(x: T, y: T, eps: T) = 
    assert{ abs(x-y) < eps }
  
  def mathTest {
    integralTests
    mathToolKitTests
    constantTest
  }
  
  def constantTest {
    assertEquals( basic.Constants.sins(30), 0.5, 1e-6)
    assertEquals( basic.Constants.coss(60), 0.5, 1e-6)
    assertEquals( basic.Constants.coss(90), 0, 1e-6)
  }
  
  def integralTests{
    assert (Integral.getN(0, 2, 0.1)  == 21)
    assert (abs(Integral.simpson(sin, 0, Pi, 0.01) - 2) < 1e-4)
    def fun(x: T, y: T) = sin(x) * sin(y)
    assertEquals( Integral.simpson2(fun, (0, 0), (Pi, Pi), 0.01),  4, 1e-4)
  }
    
  def mathToolKitTests {
    import basic.Statistic._
    import basic.Decompose._
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    assert{ abs(aver(mat1)-2.5) < 1e-10} 
    val mat2: M = Array(Array(2, 2), Array(3, 4))
    assertEquals(
            correlation(mat1, mat2),
            (31.0/4 - (10.0/4)*(11.0/4)),
            1e-10)
    assert{ decompose(254).equals( List(128, 64, 32, 16, 8, 4, 2))}
    assert{ decompose(3).equals( List(2, 1))}
    assert{ decompose(254, 32).equals( List(32, 32, 32, 32, 32, 32, 32, 16, 8, 4, 2))}
    assert{ decompose(3, 1).equals( List(1, 1, 1))}
  }
  
}

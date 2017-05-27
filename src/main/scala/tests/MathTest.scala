package tests

import basic.Integral
import basic.Basic._

import math._ 


object MathTest {
  
  def mathTest {
    decomposeTests
  }
  
  def decomposeTests{
    import basic.Decompose._
    assert{ decompose(254).equals( List(128, 64, 32, 16, 8, 4, 2))}
    assert{ decompose(3).equals( List(2, 1))}
    assert{ decomposeWithMaxBlock(254, 32).equals( List(32, 32, 32, 32, 32, 32, 32, 16, 8, 4, 2))}
    assert{ decomposeWithMaxBlock(3, 1).equals( List(1, 1, 1))}
  }  
}

package basic

import org.scalatest.FlatSpec

import math._ 
import main.Basic._
import postprocessing.Statistic._

class StatisticSpec extends FlatSpec {    
  behavior of "Primitive"
  "Averange of ((1, 2), (3, 4))" should "2.5" in {
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    assert{ abs(aver(mat1)-2.5) < 1e-10} 
  }
  
  "minMax of ((1, 10), (-2, 4))" should "(-2, 10)" in {
    val mat1: M = Array(Array(1, 10), Array(-2, 4))
    assert{ minMax(mat1) == (-2, 10) } 
  }
  
  "Correlection of ((1, 2), (3, 4)) and ((2, 2), (3, 4))" should "(31.0/4 - (10.0/4)*(11.0/4))" in {
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    val mat2: M = Array(Array(2, 2), Array(3, 4))
    assertEquals(
            correlation(mat1, mat2),
            (31.0/4 - (10.0/4)*(11.0/4)),
            1e-10)
  }
  
}
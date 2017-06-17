package postprocessing

import org.scalatest.FlatSpec
import math._
import other.Types._
import postprocessing.Statistic._

class StatisticSpec extends FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  behavior of "Primitive"
  "Averange of ((1, 2), (3, 4))" should "2.5" in {
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    assert { abs(aver(mat1) - 2.5) < 1e-10 }
  }

  "Disperion of (1, 2)" should "(1+4)/2-(3/2)^2=1/4" in {
    val ar = Array(1.0, 2.0)
    assert { abs(disp(ar, 1.5) - 0.25) < 1e-5 }
  }

  "Disperion of ((1, 2))" should "(1+4)/2-(3/2)^2=1/4" in {
    val mat = Array(Array(1.0, 2.0))
    assert { abs(disp(mat, 1.5) - 0.25) < 1e-5 }
  }

  "Disperion of ((1, 2), (3, 4))" should "(1+4+9+16)/4-(10/4)^2=120/16-100/16=20/16=10/8=5/4=1.25" in {
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    assert { abs(disp(mat1, 2.5) - 1.25) < 1e-5 }
  }

  "minMax of ((1, 10), (-2, 4))" should "(-2, 10)" in {
    val mat1: M = Array(Array(1, 10), Array(-2, 4))
    assert { minMax(mat1) == (-2, 10) }
  }

  "Correlection of ((1, 2), (3, 4)) and ((2, 2), (3, 4))" should "(31.0/4 - (10.0/4)*(11.0/4))" in {
    val mat1: M = Array(Array(1, 2), Array(3, 4))
    val mat2: M = Array(Array(2, 2), Array(3, 4))
    assertEquals(
      correlation(mat1, mat2),
      (31.0 / 4 - (10.0 / 4) * (11.0 / 4)),
      1e-10)
  }

}
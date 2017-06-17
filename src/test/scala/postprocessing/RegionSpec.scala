package postprocessing

import org.scalatest.FlatSpec
import math._
import other.Types._
import postprocessing.Region._

class RegionSpec extends FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  "region of ar=Array(10, 20, 30, 40, 50, 60, 50, 10) from ar(4) with d=20 " should
    "[2, 6] (so ar(4)-d<=ar(2)<=ar(4)+d  and  ar(4)-d<=ar(6)<=ar(4)+d)" in {
      assert { findSimilar(Array(10, 20, 30, 40, 50, 60, 50, 10), 4) == (2, 6) }
    }
}
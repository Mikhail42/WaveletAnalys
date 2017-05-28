package mainT

import org.scalatest.FlatSpec

import math._ 
import main.Basic._

object DecomposeSpec extends  FlatSpec {
    import main.Decompose._
  "254 is decompose of " should "List(128, 64, 32, 16, 8, 4, 2)" in {
    assert{ decompose(254).equals( List(128, 64, 32, 16, 8, 4, 2))}
  }
  "3 is decompose of " should "List(2, 1)" in {
    assert{ decompose(3).equals( List(2, 1))}
  }
  "254 is decompose with max block 32 of " should "List(32, 32, 32, 32, 32, 32, 32, 16, 8, 4, 2)" in {
    assert{ decomposeWithMaxBlock(254, 32).equals( List(32, 32, 32, 32, 32, 32, 32, 16, 8, 4, 2))}
  }
  "3 is decompose with max block 1 of " should "List(1, 1, 1)" in {
    assert{ decomposeWithMaxBlock(3, 1).equals( List(1, 1, 1))}
  }
}
package test

import org.scalameter.api._

object RangeBenchmark extends Bench.LocalTime {
    val sizes = Gen.range("size")(3000, 15000, 30000)
  
    val ranges = for {
      size <- sizes
    } yield 0 until size
  
    performance of "Range" in {
      measure method "map" in {
        using(ranges) in {
          r => r.map(_ + 1)
        }
      }
    }
  }
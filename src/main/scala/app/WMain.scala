package app

import basic.Basic._
import image._

object WMain {
  def main(args: Array[String]) {
    def time[R](block: => R) {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    }
    //val res = preprocessing.DilateErose.dilateErose(image.Input.getImage("/home/misha/6-1-5.jpg"))
    //image.Output.visible(res, "tit")
    time { tests.AnalysTest.vesselSegmentTest }
  }
}
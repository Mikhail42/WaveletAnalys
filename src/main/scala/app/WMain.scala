package app

import main.Basic._
import image._

object WMain {
  def main(args: Array[String]) {
    val resourcesDirectory = new java.io.File("src/main/resources")
    val dir = resourcesDirectory.getAbsolutePath() + "/"
    val forVessel = "01_g.jpg" //2//"image_023.jpg" //
    val forDisk = "1.jpg" //"image_023.jpg" //""
    val forTiff = "01_g.tif"

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
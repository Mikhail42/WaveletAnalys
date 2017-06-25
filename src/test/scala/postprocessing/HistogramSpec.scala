package postprocessing

import image.ResourcesPath
import other.Types._
import other.Time._

class HistogramSpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)
  val imgName = ResourcesPath.dir + ResourcesPath.forTiff
  val img = image.Input.uploadImage(imgName)
  val mat = imgToMInt(img)
  "histogram of matrix" should "fast" in {
    time(postprocessing.Histogram.histogram(mat), "histogram of matrix")
  }
}
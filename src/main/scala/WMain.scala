import other.Types._
import image._

object WMain {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def main(args: Array[String]) {
    logger.info("main app started")
    // other.Time.time(tests.AnalysTest.vesselSegmentTest, "Main app")
    new preprocessing.Fast_Filters().run(new ij.process.ColorProcessor(image.Input.uploadImage(image.ResourcesPath.dir + image.ResourcesPath.forDisk)))
  }
}
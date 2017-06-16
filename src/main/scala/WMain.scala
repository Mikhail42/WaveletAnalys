import other.Types._
import image._

object WMain {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def main(args: Array[String]) {
    logger.info("main app started")
    other.Time.time(tests.AnalysTest.vesselSegmentTest, "Main app")
    //other.Time.time(tests.AnalysTest.diskTest, "Main app")
  }
}
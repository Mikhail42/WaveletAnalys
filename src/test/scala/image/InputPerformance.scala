package image

import org.scalameter.api._
import ResourcesPath._
import org.slf4j.LoggerFactory

class InputPerformance extends Bench.LocalTime {
  val logger = LoggerFactory.getLogger(classOf[InputPerformance])

  def time[R](block: => R, defin: String) {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    logger.info("{}: {} ms", defin, (t1 - t0) / 1000000)
  }

  time(image.Input.uploadImage(dir + forTiff), "upload tiff")
  time(image.Input.uploadImage(dir + forDisk), "upload simple")
}
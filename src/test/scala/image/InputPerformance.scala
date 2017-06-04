package image

import org.scalameter.api._
import Constants._

object InputPerformance extends Bench.LocalTime {
  performance of "uploadImage" in {
    measure method "upload tiff" in {
      image.Input.uploadImage(dir + forTiff)
    }
    measure method "upload" in {
      image.Input.uploadImage(dir + forDisk)
    }
  }

}
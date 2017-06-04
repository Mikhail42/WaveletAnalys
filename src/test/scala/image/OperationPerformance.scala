package image

import org.scalameter.api._
import Constants._

object OperationPerformance extends Bench.LocalTime {
  performance of "Operation" in {
    measure method "to gray" in {
      Operation.toGray(Input.uploadImage(dir + forDisk))
    }
    measure method "deep copy" in {
      Operation.deepCopy(Input.uploadImage(dir + forDisk))
    }
    measure method "tiff" in {
      val mat = Input.grayMatFromImage(Input.uploadTiffImage(dir + forTiff))
      Operation.createTiffImage(mat)
    }
    measure method "simple" in {
      val mat = Input.grayMatFromImage(Input.uploadTiffImage(dir + forTiff))
      Operation.createTiffImage(mat)
    }
  }
}
package image

import org.scalameter.api._
import Constants._

object OperationPerformance extends Bench.LocalTime {
  performance of "Operation" in {
    measure method "to gray" in {
      image.Operation.toGray(image.Input.uploadImage(dir + forDisk))
    }
    measure method "deep copy" in {
      image.Operation.deepCopy(image.Input.uploadImage(dir + forDisk))
    }
    measure method "to binary" in {
      image.Operation.toBinary(image.Input.uploadImage(dir + forDisk))
    }
  }
}
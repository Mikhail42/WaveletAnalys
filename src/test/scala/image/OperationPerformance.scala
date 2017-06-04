package image

import org.scalameter.api._
import Constants._

import Operation._

object OperationPerformance extends Bench.LocalTime {
  performance of "upload pixels" in {
    measure method "pixels" in {
      image.Operation.getPixels(image.Input.uploadImage(dir + forDisk))
    }
  }

  performance of "gray image" in {
    measure method "matrix gray image" in {
      image.Operation.grayMatFromImage(image.Input.uploadImage(dir + forDisk))
    }
  }

  performance of "color matrix of image" in {
    measure method "color components" in {
      image.Operation.getColorsComponents(image.Input.uploadImage(dir + forDisk), 2)
    }
  }

  performance of "Operation" in {

    measure method "to gray" in {
      toGray(Input.uploadImage(dir + forDisk))
    }
    measure method "deep copy" in {
      deepCopy(Input.uploadImage(dir + forDisk))
    }
    measure method "tiff" in {
      val mat = grayMatFromImage(Input.uploadTiffImage(dir + forTiff))
      createTiffImage(mat)
    }
    measure method "simple" in {
      val mat = grayMatFromImage(Input.uploadTiffImage(dir + forTiff))
      createTiffImage(mat)
    }
  }
}
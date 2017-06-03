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
  performance of "upload pixels" in {
    measure method "pixels" in {
      image.Input.getPixels(image.Input.uploadImage(dir + forDisk))
    }
  }

  performance of "gray image" in {
    measure method "matrix gray image" in {
      image.Input.getMatGrayImage(image.Input.uploadImage(dir + forDisk))
    }
  }

  performance of "color matrix of image" in {
    measure method "color components" in {
      image.Input.getColorsComponents(image.Input.uploadImage(dir + forDisk), 2)
    }
  }
}
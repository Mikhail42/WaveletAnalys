package image

import ResourcesPath._

class InputPerformance {
  val logger = com.typesafe.scalalogging.Logger(getClass)
  import other.Time._

  time(image.Input.uploadImage(dir + forTiff), "upload tiff")
  time(image.Input.uploadImage(dir + forDisk), "upload simple")
}
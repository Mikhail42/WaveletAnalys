package image

import org.scalameter.api._
import org.slf4j.LoggerFactory

import ResourcesPath._
import Operation._

class OperationPerformance extends Bench.LocalTime {
  val logger = LoggerFactory.getLogger(classOf[OperationPerformance])

  val forDiskImg = Input.uploadImage(dir + forDisk)
  val forTiffImg = Input.uploadTiffImage(dir + forTiff)

  def time[R](block: => R, defin: String) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    logger.info("{}: {} ms", defin, (t1 - t0) / 1000000)
    result
  }

  time(image.Operation.getPixels(forDiskImg), "upload pixels from image")
  val grayMat = time(image.Operation.grayMatFromImage(forDiskImg), "matrix from gray image")
  val G = time(image.Operation.getColorsComponents(forDiskImg, 2), "color components #2")
  val RGBDisc = time(image.Operation.getColorsComponents(forDiskImg), "color components RGB")

  time(toGray(forDiskImg), "image to gray")

  time(toBinary(forDiskImg, 100), "image to binary")

  time(deepCopy(forDiskImg), "image deep copy")

  time(createTiffImage(grayMat), "create image from the matrix")

  time(image.Operation.createImage(RGBDisc, forDiskImg.getType), "create image from the matrix")

  time(image.Operation.rotate(forDiskImg, 30), "rotate image (30 deg) by java.awt.geom")

  {
    val mat = other.Types.imgToM(forDiskImg)
    time(image.Operation.rotate(mat, 30, mat.length, mat(0).length), "rotate matrix from me")
  }

  time(image.Operation.scale(forDiskImg, 2), "scale in 2 times")
}
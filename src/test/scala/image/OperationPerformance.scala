package image

import org.scalameter.api._
import org.slf4j.LoggerFactory

import ResourcesPath._
import Operation._

class OperationPerformance extends Bench.LocalTime {
  val logger = LoggerFactory.getLogger(classOf[OperationPerformance])

  def time[R](block: => R, defin: String) {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    logger.info("{}: {} ms", defin, (t1 - t0) / 1000000)
  }

  time(image.Operation.getPixels(image.Input.uploadImage(dir + forDisk)), "upload pixels")
  time(image.Operation.grayMatFromImage(image.Input.uploadImage(dir + forDisk)), "matrix gray image")
  time(image.Operation.getColorsComponents(image.Input.uploadImage(dir + forDisk), 2), "color components #2")
  time(image.Operation.getColorsComponents(image.Input.uploadImage(dir + forDisk)), "color components RGB")

  time(toGray(Input.uploadImage(dir + forDisk)), "to gray")

  time(deepCopy(Input.uploadImage(dir + forDisk)), "deep copy")

  time({
    val mat = grayMatFromImage(Input.uploadTiffImage(dir + forTiff))
    createTiffImage(mat)
  }, "upload tiff matrix and create image from her"
  )
  time({
    val img = image.Input.uploadImage(dir + forDisk)
    val mat = image.Operation.getColorsComponents(img)
    image.Operation.createImage(mat, img.getType)
  }, "upload all color components and create image from their"
  )

  time({
    val img = image.Input.uploadImage(dir + forDisk)
    val img2 = image.Operation.rotate(img, 30)
  }, "rotate"
  )

  time({
    val img = image.Input.uploadImage(dir + forDisk)
    val img2 = image.Operation.scale(img, 2)
  }, "scale in 2 times"
  )
}
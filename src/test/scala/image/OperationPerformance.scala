package image

import ResourcesPath._
import Convert._

class OperationPerformance {

  val forDiskImg = Input.uploadImage(dir + forDisk)
  val forTiffImg = Input.uploadTiffImage(dir + forTiff)

  import other.Time._

  time(image.Convert.getPixels(forDiskImg), "upload pixels from image")
  val grayMat = time(image.Convert.grayMatFromImage(forDiskImg), "matrix from gray image")
  val G = time(image.Convert.getColorsComponents(forDiskImg, 2), "color components #2")
  val RGBDisc = time(image.Convert.getColorsComponents(forDiskImg), "color components RGB")

  time(toBinary(forDiskImg, 100), "image to binary")

  time(deepCopy(forDiskImg), "image deep copy")

  time(createTiffImage(grayMat), "create image from the matrix")

  time(image.Convert.createImage(RGBDisc, forDiskImg.getType), "create image from the matrix")

  time(image.GeomAffineTransform.rotate(forDiskImg, 30), "rotate image (30 deg) by java.awt.geom")

  {
    val mat = other.Types.imgToM(forDiskImg)
    time(image.GeomAffineTransform.rotate(mat, 30, mat.length, mat(0).length), "rotate matrix from me")
  }

  time(image.GeomAffineTransform.scale(forDiskImg, 2), "scale in 2 times")
}
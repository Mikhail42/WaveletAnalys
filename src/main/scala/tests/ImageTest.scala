package tests

import main.Basic._

import math._
import image._

import image.Operation._

import Base._

object ImageTest {

  def imp = {import main.Basic._}
  /** test on the allocation of the field direction */
  def directionTest {
    val name = dir + forDirection
    val img = Input.uploadImage(name)
    val direct: BI = postprocessing.Direction.direction(img)
    Output.saveImage(direct, s"${name}_field_out.jpg", Input.defaultFormat)
  }

  /** test load, scale and visible .tif image */
   def loadTiffTest {
    val name = dir + forTiff
    val img: BI = Input.uploadTiffImage(name)
    val resImg = Operation.scale(img, 1000.0/img.getWidth)
    Output.visible(resImg, "Test load a tif image")
  }

  /** compare two gray images. 2th image is ideal, 1th -- is my result */
  def compareGrayWithTiffTest {
    val imgWT = Input.uploadImage(dir+forComp1)
    val resWt = Operation.toBinary(imgWT)

    val imgTif: BI = Input.uploadTiffImage(dir+forComp2)

    val resCompare = postprocessing.Compare.compareBinaryImages(resWt, imgTif)
    println(s"Compare Image Test: result = $resCompare")
  }

  /** img <- img & mask */
  def maskTest{
    val inpName = dir + forMask
    val img = Input.uploadImage(inpName)

    val inpNameMask = dir+forFullMask
    val tif: BI = Input.uploadImage(inpNameMask)
    val tifScala: BI = Operation.scale(tif, 600.0/tif.getWidth)
    Output.saveImage(tifScala, dir+"g_mask.jpg", "jpg")
    val mask = Input.uploadImage(inpNameMask)

    val colComps = getColorsComponents(img, 0.0, 0.3, 0.7).map{_.map{255 - _}}
    for (i <- 0 until mask.getHeight; j <- 0 until mask.getWidth)
      colComps(i)(j) = colComps(i)(j) & mask.getRGB(j, i)
    val greenImg = Operation.createTiffImage(colComps)
    preprocessing.Filter.constrast(greenImg)
    val resImg = Operation.scale(greenImg, 600.0/greenImg.getWidth)
    Output.saveImage(resImg, dir+forMaskOut, "jpg")
    Output.visible(resImg, "Mask Test")
  }
}
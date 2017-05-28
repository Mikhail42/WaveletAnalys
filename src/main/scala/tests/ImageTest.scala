package tests

import basic.Basic._


import math._ 
import image._

import Base._


object ImageTest {
    
  def imp = {import basic.Basic._}
  /** test on the allocation of the field direction */
  def directionTest {
    val name = dir + forDirection
    val img = Input.getImage(name) 
    val direct: BI = Analys.direction(img)
    Output.saveImage(direct, s"${name}_field_out.jpg", Input.format)
  }
  
  /** test load, scale and visible .tif image */
   def imageTifTest {
    val name = dir + forTiff
    val img: BI = Input.getTifImage(name)
    val resImg = Operation.scale(img, 1000.0/img.getWidth)
    Output.visible(resImg, "Test load a tif image")
  }
  
  /** compare two gray images. 2th image is ideal, 1th -- is my result */
  def compareTest {
    val imgWT = Input.getImage(dir+forComp1)
    val resWt = Operation.toBinary(imgWT)
    
    val imgTif: BI = Input.getTifImage(dir+forComp2)
    
    val resCompare = Analys.compareBinaryImages(resWt, imgTif)
    println(s"Compare Image Test: result = $resCompare")
  }
  
  /** img <- img & mask */
  def maskTest{    
    val inpName = dir + forMask
    val img = Input.getImage(inpName)    
   
    val inpNameMask = dir+forFullMask
    val tif: BI = Input.getImage(inpNameMask)
    val tifScala: BI = Operation.scale(tif, 600.0/tif.getWidth)
    Output.saveImage(tifScala, dir+"g_mask.jpg", "jpg")
    val mask = Input.getImage(inpNameMask)
    
    val colComps = Input.getColorsComponents(img, 0.0, 0.3, 0.7).map{_.map{255 - _}}
    for (i <- 0 until mask.getHeight; j <- 0 until mask.getWidth)
      colComps(i)(j) = colComps(i)(j) & mask.getRGB(j, i)
    val greenImg = Operation.toImage(colComps)
    preprocessing.Filter.constrast(greenImg)
    val resImg = Operation.scale(greenImg, 600.0/greenImg.getWidth)
    Output.saveImage(resImg, dir+forMaskOut, "jpg")
    Output.visible(resImg, "Mask Test")
  }
}
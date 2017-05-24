package tests

import basic.Basic._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import math._ 
import image._

@RunWith(classOf[JUnitRunner])
object ImageTest {
  // basic directory of images
  val dir = "/home/misha/"
    
  def imp = {import basic.Basic._}
  /** test on the allocation of the field direction */
  def directionTest {
    val name = dir + "126.jpg"
    val img = Input.getImage(name) 
    val direct: BI = Analys.direction(img)
    Output.saveImage(direct, s"${name}_field_out.jpg", Input.format)
  }
  
  /** test load, scale and visible .tif image */
 //TODO
  /* def imageTifTest {
    val name = dir + "01_g.tif"
    val img: BI = Input.getTifImage(name)
    val resImg = Operation.scale(img, 1000.0/img.getWidth)
    Output.visible(resImg, "Test load a tif image")
  }
  
  /** compare two gray images. 2th image is ideal, 1th -- is my result */
  def compareTest {
    val imgWT = Input.getImage(dir+"02_dr_out.jpg")
    val resWt = Operation.toBinary(imgWT)
    
    val imgTif: BI = Input.getTifImage(dir+"02_dr.tif")
    
    val resCompare = Analys.compareBinaryImages(resWt, imgTif)
    println(s"Compare Image Test: result = $resCompare")
  }
  */
  /** img <- img & mask */
  def maskTest{    
    val inpName = dir + "01_g_full.jpg"
    val img = Input.getImage(inpName)    
   
    val inpNameMask = dir+"01_g_mask_full.jpg"
    val tif: BI = Input.getImage(inpNameMask)
    val tifScala: BI = Operation.scale(tif, 600.0/tif.getWidth)
    Output.saveImage(tifScala, dir+"g_mask.jpg", "jpg")
    val mask = Input.getImage(inpNameMask)
    
    val colComps = Input.getColorsComponents(img, 0.0, 0.3, 0.7).map{_.map{255 - _}}
    for (i <- 0 until mask.getHeight; j <- 0 until mask.getWidth)
      colComps(i)(j) = colComps(i)(j) & mask.getRGB(j, i)
    val greenImg = Operation.toImage(colComps)
    preprocessing.Filtr.constrast(greenImg)
    val resImg = Operation.scale(greenImg, 600.0/greenImg.getWidth)
    Output.saveImage(resImg, dir+"01_g.jpg","jpg")
    Output.visible(resImg, "Mask Test")
  }
}
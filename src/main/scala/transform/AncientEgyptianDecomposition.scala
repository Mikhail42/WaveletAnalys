package transform

import other.Types._
import other.ArrayToolKit._

class AncientEgyptianDecomposition(wavelet: wavelets.WaveletTransformTrait) extends TransformTrait {
  override val logger = com.typesafe.scalalogging.Logger(getClass)

  val wpTransform = new WaveletPacketTransform(wavelet)

  private def oneD(array1: A, fun: A => A, maxLvl: Int = 8): A = {
    logger.info(s"one dimention transform with max level is ${maxLvl}")
    val n = array1.length
    val array2 = new A(n)
    var offSet = 0
    val lvls = Decompose.decomposeWithMaxBlock(n, maxLvl)
    for (x <- lvls) {
      val arr1Sub: A = copyPart(array1, offSet, x)
      val arr2Sub: A = fun(arr1Sub)
      Array.copy(arr2Sub, 0, array2, offSet, x)
      offSet += x
    }
    array2
  }

  override def forward1D(arrTime: A) =
    oneD(arrTime, wpTransform.forward1D)
  override def reverse1D(arrToReverse: A) =
    oneD(arrToReverse, wpTransform.reverse1D)

  override def forward1D(arrToForward: A, level: Int): A = forward1D(arrToForward)
  override def reverse1D(arrToReverse: A, level: Int): A = reverse1D(arrToReverse)
}
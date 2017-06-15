package transform

import other.Types._
import other.ArrayToolKit._
import exceptions._
import wavelets.WaveletTransformTrait

class WaveletPacketTransform(wavelet: WaveletTransformTrait) extends TransformTrait {
  override val logger = com.typesafe.scalalogging.Logger(getClass)

  override def reverse1D(arrToReverse: A, level: Int): A = {
    logger.info(s"reverse1D")
    val n = arrToReverse.length
    val arrTime = arrToReverse.clone()
    val steps = log2(n)
    if (!isBinary(n))
      throw new BinaryAmountException("arrToReverse", n)
    var h = wavelet.transformWavelength << (steps - level)
    while (h <= arrTime.length && h >= wavelet.transformWavelength) {
      val g = n / h // ... -> 8 -> 4 -> 2 -> 1
      val iBuf = new A(h)
      for (p <- 0 until g) {
        copyArray(arrTime, p * h, iBuf, 0, h)
        val oBuf = wavelet.waveletReverse(iBuf, h)
        copyArray(oBuf, 0, arrTime, p * h, h)
      }
      h <<= 1
    }
    arrTime
  }

  override def forward1D(arrTime: A, level: Int): A = {
    logger.info(s"forward1D")
    val n = arrTime.length
    if (!isBinary(n))
      throw new BinaryAmountException(s"arrTime", n)
    val arrHilb = arrTime.clone()
    var h = n
    var l = 0
    while (l < level && h >= wavelet.transformWavelength) {
      val g = n / h
      val iBuf = new A(h)
      for (p <- 0 until g) {
        copyArray(arrHilb, p * h, iBuf, 0, h)
        val oBuf = wavelet.waveletForward(iBuf, h)
        copyArray(oBuf, 0, arrHilb, p * h, h)
      }
      h >>= 1
      l += 1
    }
    arrHilb
  }
}
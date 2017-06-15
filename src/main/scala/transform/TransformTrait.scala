package transform

import other.Types._
import other.ArrayToolKit._
import exceptions._

/** @author  Mikhail   Ionkin     (ionkinmikhail@gmail.com)
 *
 *    I'm using a project
 *   [1] JWave (by Christian Scheiblich (cscheiblich@gmail.com))
 *  @see  https://github.com/cscheiblich/JWave
 */
trait TransformTrait {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** [1]:
   *  Performs the reverse transform from frequency or Hilbert domain to time
   *  domain for a given array depending on the used transform algorithm by
   *  inheritance.
   */
  def reverse1D(arrToReverse: A): A = {
    logger.info(s"reverse1D")
    val n = arrToReverse.length
    if (!isBinary(n))
      throw new BinaryAmountException("arrToReverse", n)
    reverse1D(arrToReverse, log2(n))
  }
  /** @see reverse1D(arrToReverse) */
  def reverse1D(arrToReverse: A, level: Int): A

  /** [1]:
   *  Performs the forward transform from time domain to frequency or Hilbert
   *  domain for a given array depending on the used transform algorithm by
   *  inheritance.
   */
  def forward1D(arrToForward: A): A = {
    logger.info(s"forward1D")
    val n = arrToForward.length
    if (!isBinary(n))
      throw new BinaryAmountException("arrToForward", n)
    forward1D(arrToForward, log2(n))
  }
  /** @see forward1D(arrToForward) */
  def forward1D(arrToForward: A, level: Int): A

  protected def twoOnString(inp: M, lvlM: Int, lvlN: Int, fun1D: (A, Int) => A): M =
    for (str <- inp) yield fun1D(str, lvlN)

  protected def twoOnColumn(inp: M, lvlM: Int, lvlN: Int, fun1D: (A, Int) => A): M = {
    val res = createM(inp.length, inp(0).length)
    for (j <- 0 until inp(0).length)
      copyColumn(fun1D(copyColumn(inp, j), lvlM), res, j)
    res
  }

  /** @param transformID:
   *      str -- transform of strings
   *      col -- transform of columns
   *    mat -- transform of strings and columns
   */
  protected def two(mat1: M, lvlM: Int, lvlN: Int, fun1D: (A, Int) => A, transformID: String): M =
    transformID match {
      case "str" =>
        for (str <- mat1) yield fun1D(str, lvlN)
      case "col" => {
        val res = createM(mat1.length, mat1(0).length)
        for (j <- 0 until mat1(0).length)
          copyColumn(fun1D(copyColumn(mat1, j), lvlM), res, j)
        res
      }
      case "mat" => {
        val res1 = two(mat1, lvlM, lvlN, fun1D, "str")
        val res2 = two(res1, lvlM, lvlN, fun1D, "col")
        res2
      }
    }

  /** [1]
   *  Performs the 2-D forward transform from time domain to frequency or Hilbert
   *  domain for a given matrix depending on the used transform algorithm by
   *  inheritance.
   */
  def forward2D(matTime: M): M =
    forward2D(matTime, log2(matTime.length), log2(matTime(0).length))

  /** @see forward2D(matTime) */
  def forward2D(matTime: M, transformID: String): M =
    forward2D(matTime, log2(matTime.length), log2(matTime(0).length), transformID)

  /** @see forward2D(arrToForward) */
  def forward2D(matTime: M, lvlM: Int, lvlN: Int): M =
    two(matTime, lvlM, lvlN, forward1D, "mat")

  /** @see forward2D(arrToForward) */
  def forward2D(matTime: M, lvlM: Int, lvlN: Int, transformID: String): M =
    two(matTime, lvlM, lvlN, forward1D, transformID)

  /** [1]
   *  Performs the 2-D reverse transform from frequency or Hilbert or time domain
   *  to time domain for a given matrix depending on the used transform algorithm
   *  by inheritance.
   */
  def reverse2D(matFreq: M): M =
    reverse2D(matFreq, log2(matFreq.length), log2(matFreq(0).length))

  /** @see reverse2D(arrToReverse) */
  def reverse2D(matFreq: M, lvlM: Int, lvlN: Int): M =
    two(matFreq, lvlM, lvlN, reverse1D, "mat")

  /** @see reverse2D(matFreq) */
  def reverse2D(matFreq: M, transformID: String): M =
    reverse2D(matFreq, log2(matFreq.length), log2(matFreq(0).length), transformID)

  /** @see reverse2D(arrToReverse) */
  def reverse2D(matFreq: M, lvlM: Int, lvlN: Int, transformID: String): M =
    two(matFreq, lvlM, lvlN, reverse1D, transformID)
  /** [1]
   *  Generates from a 2-D decomposition a 1-D time series.
   */
  def decompose(times: A): M = ???
  /** @see decompose(times) */
  def decompose(times: A, level: Int): M = ???

  /** [1]
   *  Generates from a 1-D signal a 2-D output, where the second dimension are
   *  the levels of the wavelet transform. The first level should keep the
   *  original coefficients. All following levels should keep each step of the
   *  decomposition of the Fast Wavelet Transform. However, each level of the
   *  this decomposition matrix is having the full set, full energy and full
   *  details, that are needed to do a full reconstruction. So one can select a
   *  level filter it and then do reconstruction only from this single line!
   *  BY THIS METHOD, THE _HIGHEST_ LEVEL IS _ALWAYS_ TAKEN FOR RECONSTRUCTION!
   */
  def recompose(matDecompose: M): A = ???

  /** recompose(matDecompose) */
  def recompose(matDecompose: M, level: Int): A = ???
}
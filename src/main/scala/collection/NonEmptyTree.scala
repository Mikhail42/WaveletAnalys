package collection

import math._
import other.Types._

object NonEmptyTree {
  // O(1)
  def apply(data: Int) = new NonEmptyTree(data, EmptyTree, EmptyTree, 1)
  // O(1)
  def apply(data: Int, left: BinaryTree, right: BinaryTree, reCount: Int) = new NonEmptyTree(data, left, right, reCount)
}

case class NonEmptyTree(val data: Int, private var left: BinaryTree, private var right: BinaryTree, private var reCount: Int)
    extends BinaryTree {
  // O(1)
  override def count = length
  // O(log2(256))
  override def deep = 1 + math.max(left.deep, right.deep)

  // O(1)
  var length = reCount + left.count + right.count

  // O(log2(256))
  private def setLeft(left: BinaryTree) {
    this.left = left
    length = reCount + left.count + right.count
  }
  // O(log2(256))
  private def setRight(right: BinaryTree) {
    this.right = right
    length = reCount + left.count + right.count
  }
  // O(1)
  private def setReCount(reCount: Int) {
    length += reCount - this.reCount
    this.reCount = reCount
  }

  // O(2*log2(256))
  override def add(a: Int): NonEmptyTree = {
    if (a > data) setLeft(left.add(a))
    else if (a < data) setRight(right.add(a))
    else setReCount(reCount + 1)
    this
  }
  // O(2*log2(256))
  override def add(a: Int, re: Int): NonEmptyTree = {
    if (a > data) setLeft(left.add(a, re))
    else if (a < data) setRight(right.add(a, re))
    else setReCount(reCount + re)
    this
  }

  // O(ar.length*2*log2(256))
  def ++=(ar: AInt): NonEmptyTree = {
    ar.foreach { x => add(x) }
    this
  }
  // O(ar.length*2*log2(256))
  def --=(ar: AInt): NonEmptyTree = {
    ar.foreach { x => remove(x) }
    this
  }

  // O(4*log2(256))
  def addSymmetrically(a: Int): NonEmptyTree =
    add(data - a).add(data + a)

  // O(2*log2(256))
  override def remove(a: Int): NonEmptyTree = {
    if (a > data) setLeft(left.remove(a))
    else if (a < data) setRight(right.remove(a))
    else if (reCount > 0) setReCount(reCount - 1)
    else throw new Exception("Can't remove unexisted element")
    this
  }
  // O(2*log2(256))
  override def remove(a: Int, count: Int): NonEmptyTree = {
    if (a > data) setLeft(left.remove(a, count))
    else if (a < data) setRight(right.remove(a, count))
    else if (reCount > count) setReCount(reCount - count)
    else throw new Exception("Can't remove unexisted element")
    this
  }

  // O(log2(256))
  def nth(i: Int): Int = {
    if (i < 0) throw new Exception("Index must be more than zero")
    if (i < left.count) left.asInstanceOf[NonEmptyTree].nth(i)
    else if (i < left.count + reCount) data
    else right.asInstanceOf[NonEmptyTree].nth(i - left.count - reCount)
  }

  override def toList: List[Int] = left.toList ::: Array.fill[Int](reCount)(data).toList ::: right.toList

  override def equals(a: Any) =
    a.isInstanceOf[NonEmptyTree] && (toList equals a.asInstanceOf[NonEmptyTree].toList)

  override def toString = "(" + left + ", " + data + (if (reCount != 1) "re" + reCount else "") + ", " + right + ")"
}
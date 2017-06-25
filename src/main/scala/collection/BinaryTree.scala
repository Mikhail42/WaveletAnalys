package collection

abstract class BinaryTree {
  // O(1)
  def count: Int = if (this == EmptyTree) 0 else asInstanceOf[NonEmptyTree].length
  def deep(): Int
  def nth(i: Int): Int

  def add(a: Int): NonEmptyTree
  // O(add)
  def +=(a: Int) = add(a)
  def add(a: Int, re: Int): NonEmptyTree
  // O(add)
  def +=(a: Int, re: Int) = add(a, re)

  def remove(a: Int): NonEmptyTree
  def remove(a: Int, re: Int): NonEmptyTree
  // O(remove)
  def -=(a: Int) = remove(a)
  // O(remove)
  def -=(a: Int, re: Int) = remove(a, re)

  def toList(): List[Int]
}
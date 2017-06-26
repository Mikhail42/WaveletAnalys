package collection

class EmptyTreeSpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  "EmptyTree is EmptyTree" should "true" in {
    assert(EmptyTree.equals(EmptyTree))
  }
  "EmptyTree is not NonEmptyTree.+=(1).-=(1)" should "true" in {
    assert(!EmptyTree.equals(NonEmptyTree(1).-=(1)))
  }
  "EmptyTree.add(1) is NonEmptyTree(1). WARN: EmptyTree is singleton and constant, but NonEmtyTree is mutable" should "true" in {
    assert(EmptyTree.add(1).equals(NonEmptyTree(1)))
  }
}
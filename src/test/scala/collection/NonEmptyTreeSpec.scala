package collection

class NonEmptyTreeSpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  "(., 128, .) += 64" should "(., 128, (., 64, .))" in {
    assert(NonEmptyTree(128).+=(64).equals(NonEmptyTree(128, EmptyTree, NonEmptyTree(64), 1)))
    assert(NonEmptyTree(128).+=(64).+=(96)
      .equals(
        NonEmptyTree(128,
          EmptyTree, NonEmptyTree(64,
            NonEmptyTree(96), EmptyTree, 1), 1)))
  }

  "NonEmptyTree(128).+=(64).+=(96).+=(1))" should "(., 128, ((., 96, .), 64, (., 1, .)))" in {
    NonEmptyTree(128).+=(64).+=(96).+=(1).toString == "(., 128, ((., 96, .), 64, (., 1, .)))"
  }
}
package exceptions

class AmountItemsException(msg: String, cause: Throwable = null)
  extends java.lang.Exception (msg, cause) {
  val _msg = s"The number of items error: ${msg}"
  override def getMessage = s"The number of items error: ${msg}"
}
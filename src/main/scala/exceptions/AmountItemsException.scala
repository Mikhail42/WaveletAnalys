package exceptions

object AmountItemsException {
  val logger = com.typesafe.scalalogging.Logger(getClass)
}

class AmountItemsException(msg: String, cause: Throwable = null)
    extends java.lang.Exception(msg, cause) {
  AmountItemsException.logger.error(msg, cause)
  val _msg = s"The number of items error: ${msg}"
  override def getMessage = s"The number of items error: ${msg}"
}
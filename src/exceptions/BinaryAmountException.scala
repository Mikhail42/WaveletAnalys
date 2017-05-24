package exceptions

class BinaryAmountException(valueName: String, n: Int, msg: String=null, cause: Throwable=null)
    extends AmountItemsException(msg, cause) {
  override val _msg = 
    s"The number of items error: ${valueName}.length=${n} must be 2^k, where k is integer"
  override def getMessage = _msg
}
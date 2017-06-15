package exceptions

object ImageInfoException {
  val logger = com.typesafe.scalalogging.Logger(getClass)
}

class ImageInfoException(msg: String, cause: Throwable = null)
    extends java.lang.Exception(msg, cause) {
  ImageInfoException.logger.error(msg, cause)
}
package wdl4s.exception

case class ValidationException(exceptionContext: String, throwables: List[Throwable]) extends ThrowableAggregation
package wdl4s.exception

import java.io.FileNotFoundException
import java.nio.file.NoSuchFileException

/**
  * When mixed in an Exception class,
  * aggregates multiple error messages into the getMessage method.
  */
trait MessageAggregation extends Throwable {
  def exceptionContext: String
  def errorMessages: Traversable[String]

  override def getMessage = {
    val messages = if(errorMessages.nonEmpty) s"\n${errorMessages.mkString("\n")}" else ""
    s"""$exceptionContext$messages"""
  }
}

/**
  * When mixed in an Exception class,
  * aggregates multiple throwables into the extended Exception.
  */
trait ThrowableAggregation extends MessageAggregation {
  def throwables: Traversable[Throwable]

  throwables foreach addSuppressed

  override def errorMessages = throwables map buildMessage
  
  private def buildMessage(t: Throwable): String = {
    t match {
      case aggregation: ThrowableAggregation =>
        val children = aggregation.throwables.map(buildMessage).mkString("\n")
        s"${aggregation.exceptionContext}: $children"
      case _: FileNotFoundException | _: NoSuchFileException => s"File not found ${t.getMessage}"
      case other => other.getMessage
    }
  }
}

/**
  * Generic convenience case class for aggregated exceptions.
  */
case class AggregatedException(exceptionContext: String, throwables: Traversable[Throwable]) extends Exception with ThrowableAggregation

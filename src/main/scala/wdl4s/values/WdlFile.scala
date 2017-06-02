package wdl4s.values

import com.google.common.net.UrlEscapers
import wdl4s.types.{WdlFileType, WdlType}
import eu.timepit.refined.refineV
import eu.timepit.refined.string.Uri
import wdl4s.exception.UnsatisfiedInputException

import scala.util.{Success, Try}

object WdlFile {
  def appendPathsWithSlashSeparators(path1: String, path2: String): String = {
    if (path1.endsWith("/") || path2.startsWith("/")) path1 + path2
    else path1 + "/" + path2
  }

  def apply(value: String, isGlob: Boolean = false): WdlFile = if (isGlob) WdlGlobFile(value) else WdlSingleFile(value)
}

sealed trait WdlFile extends WdlPrimitive {
  val value: String
  val wdlType: WdlType = WdlFileType

  def isGlob: Boolean = this match {
    case _: WdlGlobFile => true
    case _ => false
  }

  override def add(rhs: WdlValue): Try[WdlValue] = rhs match {
    case r: WdlString => Success(WdlFile(value + r.value))
    case r: WdlOptionalValue => evaluateIfDefined("+", r, add)
    case _ => invalid(s"$value + $rhs")
  }

  override def equals(rhs: WdlValue): Try[WdlBoolean] = rhs match {
    case r: WdlFile => Success(WdlBoolean(value.equals(r.value) && isGlob.equals(r.isGlob)))
    case r: WdlString => Success(WdlBoolean(value.toString.equals(r.value.toString) && !isGlob))
    case r: WdlOptionalValue => evaluateIfDefined("==", r, equals)
    case _ => invalid(s"$value == $rhs")
  }

  override def valueString: String = value.toString
}

case class WdlSingleFile(origValue: String) extends WdlFile {
  override def toWdlString: String = "\"" + value.toString + "\""

  // Validate file path
  val value: String = refineV[Uri](UrlEscapers.urlFragmentEscaper().escape(origValue)) match {
    case Left(_) => throw new UnsatisfiedInputException(s"String: '$origValue' is not a valid URI")
    case Right(_) => origValue
  }
}

case class WdlGlobFile(value: String) extends WdlFile {
  override def toWdlString: String = "glob(\"" + value.toString + "\")"
}


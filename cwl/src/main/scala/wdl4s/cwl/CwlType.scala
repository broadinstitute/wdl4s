package wdl4s.cwl

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import shapeless.{:+:, CNil}
import shapeless.syntax.singleton._

object CwlType extends Enumeration {
  type CwlType = Value

  val Null = Value("null")
  val Boolean = Value("boolean")
  val Int = Value("int")
  val Long = Value("long")
  val Float = Value("float")
  val Double = Value("double")
  val String = Value("string")
  val File = Value("File")
  val Directory = Value("Directory")
}

case class File private(
  `class`: W.`"File"`.T,
  location: Option[String], //TODO refine w/ regex  of IRI
  path: Option[String],
  basename: Option[String],
  dirname: Option[String],
  nameroot: Option[String],
  nameext: Option[String],
  checksum: Option[String],
  size: Option[Long],
  secondaryFiles: Option[Array[File :+: Directory :+: CNil]],
  format: Option[String],
  contents: Option[String])

object File {
  def apply(
             location: Option[String] = None, //TODO refine w/ regex  of IRI
             path: Option[String] = None,
             basename: Option[String] = None,
             dirname: Option[String] = None,
             nameroot: Option[String] = None,
             nameext: Option[String] = None,
             checksum: Option[String] = None,
             size: Option[Long] = None,
             secondaryFiles: Option[Array[File :+: Directory :+: CNil]] = None,
             format: Option[String] = None,
             contents: Option[String] = None): File =
    new wdl4s.cwl.File(
       "File".narrow,
       location,
       path,
       basename,
       dirname,
       nameroot,
       nameext,
       checksum,
       size,
      secondaryFiles,
      format,
      contents
    )
}

case class Directory(
  `class`: String Refined MatchesRegex[W.`"Directory"`.T],
  location: Option[String],
  path: Option[String],
  basename: Option[String],
  listing: Option[Array[File :+: Directory :+: CNil]])



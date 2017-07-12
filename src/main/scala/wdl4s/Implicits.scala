package wdl4s.cwl

import io.circe._
import io.circe.generic.auto._
import io.circe.yaml.{parser => YamlParser}

import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.auto._
import shapeless.{:+:, CNil, Coproduct}
import cats.syntax.either._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.string._
import eu.timepit.refined._
import io.circe.refined._
import io.circe.literal._
import cats.data.ValidatedNel
import cats.data.Validated._
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.option._

trait Implicits {
  import CwlType._
  import CwlVersion._
  import ScatterMethod._
  import RequirementClass._


  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)
  implicit val argumentClass = Decoder.enumDecoder(RequirementClass)
  implicit val scatterMethodEncoder = Encoder.enumEncoder(ScatterMethod)

}

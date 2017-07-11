package wdl4s.cwl

/*
import io.circe._
//import io.circe.generic.auto._
import io.circe.generic.extras.defaults._
import io.circe.generic.extras.auto._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras._
import io.circe.parser._
import io.circe.syntax._
//import EnvVarRequirement

//import io.circe.generic.semiauto._
//import io.circe.generic.extras.semiauto._

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string._
import io.circe.shapes._
import io.circe.refined._
*/
import wdl4s.cwl._
import io.circe.syntax._
import io.circe._
import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.extras.auto._
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.extras.defaults._
import io.circe.generic.extras.decoding.ConfiguredDecoder
import io.circe.generic.extras.semiauto._
import io.circe.yaml.{parser => YamlParser}
import io.circe.Json
import io.circe.syntax._
import io.circe._
import io.circe.parser._
import io.circe.shapes._
import shapeless._
import poly._
import shapeless.ops.coproduct._
import cats._
import implicits._
import io.circe._
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.string._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import io.circe.Decoder.Result

import scala.util.Try
import io.circe.refined._
import cats.data.ValidatedNel
import cats.data.Validated._
import io.circe.literal._

object some {
  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)

  import CwlType._
  import CwlVersion._
  import ScatterMethod._

  type EVR = W.`"EnvVarRequirement"`.T => EnvVarRequirement
  type IJR = W.`"InlineJavascriptRequirement"`.T => InlineJavascriptRequirement
  type SR = W.`"SoftwareRequirement"`.T => SoftwareRequirement
  type SFT = W.`"SubworkflowFeatureRequirement"`.T => SubworkflowFeatureRequirement
  type SDR = W.`"SchemaDefRequirement"`.T => SchemaDefRequirement
  type DR = W.`"DockerRequirement"`.T => DockerRequirement
  type IWDR = W.`"InitialWorkDirRequirement"`.T => InitialWorkDirRequirement
  type SCR = W.`"ShellCommandRequirement"`.T => ShellCommandRequirement
  type RR = W.`"ResourceRequirement"`.T => ResourceRequirement
  type SFR = W.`"ScatterFeatureRequirement"`.T => ScatterFeatureRequirement
  type MIFR = W.`"MultipleInputFeatureRequirement"`.T => MultipleInputFeatureRequirement
  type SIER = W.`"StepInputExpressionRequirement"`.T => StepInputExpressionRequirement

  type Target =
    EVR :+:
    IJR :+:
    SR :+:
    SFT :+:
    SDR :+:
    DR :+:
    IWDR :+:
    EVR :+:
    SCR :+:
    RR :+:
    SFR :+:
    MIFR :+:
    SIER :+:
    CNil

  implicit val envvarD = deriveDecoder[EVR]
  implicit val ijr = Decoder[IJR]
  implicit val sr = Decoder[SR]
  implicit val sw = Decoder[SFT]
  implicit val sdr = deriveDecoder[SDR]
  implicit val dr = Decoder[DR]
  implicit val iwdr = deriveDecoder[IWDR]
  implicit val scr = Decoder[SCR]
  implicit val rr = deriveDecoder[RR]
  implicit val sfr = Decoder[SFR]
  implicit val mifr = Decoder[MIFR]
  implicit val sier = Decoder[SIER]
  implicit val X = Decoder[Map[String, Target]]

  type V[A] = ValidatedNel[String,A]

  //FAILSAFE
  //implicit val parser2: Decoder[Array[Requirement]] =
   // Decoder[Map[String, EVR]].map{_ => println("b"); Array.empty}

  implicit val parser: Decoder[Array[Requirement]] =
    Decoder[Map[String, Target]].
        emap {
          m =>
           m.toList.traverse[V,Requirement] {
             case ("EnvVarRequirement", valueFns) => valueFns.select[EVR].toValidNel("wrong").map(_("EnvVarRequirement")).map(Coproduct[Requirement](_))
             case (_,__) => invalidNel("bad")
           }.toEither.leftMap(_.toList.mkString(", ")).map(_.toArray)
        }
 /*

  implicit val parser: Decoder[Array[Requirement]] = {

    Decoder[Map[String, EnvVarRequirement]].map{_ => println("using it"); Array.empty}
  }



  val parser: Decoder[Array[Requirement]] = {

    val d: Decoder[Array[Requirement]] = Decoder[Map[String, Target]].
        emap {
           _.toList.traverse {
             case (key, valueFns) => valueFns.select[EVR].toRight("wrong").flatMap(fn => refineV(key)().map(fn)).toValidatedNel
           }.toEither.leftMap(_.mkString(", "))
        }
        ???
  }
  */


 //def z: Decoder[Map[String,

  /*
  implicit val envvarDecoder = Decoder[EnvVarRequirement]
  implicit val envvarDecode00 = Decoder[String Refined MatchesRegex[W.`"EnvVarRequirement"`.T]]
  implicit val envvarDecoder2 = Decoder[String Refined MatchesRegex[W.`"EnvVarRequirement"`.T] => EnvVarRequirement]
 type S = String Refined MatchesRegex[W.`"EnvVarRequirement"`.T] => EnvVarRequirement

 */
  /*

  implicit val envvarDecoder4 = Decoder[Map[String, String]]

  //implicit val envvarDecoder3 = Decoder[Map[String Refined MatchesRegex[W.`"EnvVarRequirement"`.T], String Refined MatchesRegex[W.`"EnvVarRequirement"`.T] => EnvVarRequirement]]
  implicit val envvarDecoder3 = Decoder[Map[String, String Refined MatchesRegex[W.`"EnvVarRequirement"`.T] => EnvVarRequirement]]
  */




}

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

package wdl4s

/*
<<<<<<< HEAD
import wdl4s.cwl._
import io.circe.syntax._
import io.circe._
import io.circe.parser._
import io.circe.shapes._
import io.circe.generic.extras.auto._
import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.extras.defaults._
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
import cats.data.Kleisli
import io.circe._
import eu.timepit.refined.string._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import io.circe.Decoder.Result

import scala.util.Try
=======
  */
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

/**
 * This package is intended to parse all CWL files.
 *
 * =Usage=
 * {{{
 * import wdl4s.cwl._
 *
 * val firstTool = """
 * cwlVersion: v1.0
 * class: CommandLineTool
 * baseCommand: echo
 * inputs:
 *   message:
 *     type: string
 *     inputBinding:
 *       position: 1
 * outputs: []
 * """
 * decodeCwl(firstTool) //returns Either[Error, Cwl]
 * }}}
 *
 *
 * It makes heavy use of Circe YAML/Json auto derivation feature and
 * Circe modules that support the Scala libraries shapeless and Refined.
 *
 * The [[https://oss.sonatype.org/service/local/repositories/releases/archive/com/chuusai/shapeless_2.12/2.3.2/shapeless_2.12-2.3.2-javadoc.jar/!/shapeless/Coproduct.html shapeless.coproduct]] feature allows us to specify a large
 * number of potential types to be parsed.  A.k.a. a "disjunction" or "OR" relationship amongst these types.
 *
 * The [[https://github.com/fthomas/refined/blob/master/modules/core/shared/src/main/scala/eu/timepit/refined/string.scala MatchesRegex]] "refined type" is used
 * to enforce structure upon String values in the CWL Yaml.  Shapeless' Witness type
 * is used to declare a type containing a String literal.
 *
 * @see <a href="http://www.commonwl.org/v1.0/">CWL Specification</a>
 * @see <a href="https://github.com/circe/circe">circe</a>
 * @see <a href="https://github.com/circe/circe-yaml">circe-yaml</a>
 * @see <a href="https://github.com/fthomas/refined">Refined</a>
 * @see <a href="https://github.com/milessabin/shapeless">Shapeless</a>
 */
package object cwl extends TypeAliases with Implicits {

  type ECMAScriptExpression = String Refined MatchesRegex[W.`"$({.*}|{.*})"`.T]

  type Yaml = String

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
    SCR :+:
    RR :+:
    SFR :+:
    MIFR :+:
    SIER :+:
    CNil

  implicit val envvarD = Decoder[EVR]
  implicit val ijr = Decoder[IJR]
  implicit val sr = Decoder[SR]
  implicit val sw = Decoder[SFT]
  implicit def sdr = Decoder[SDR]
  implicit def rr = Decoder[RR]
  implicit def iwdr = Decoder[IWDR]
  implicit val dr = Decoder[DR]
  implicit val scr = Decoder[SCR]
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

  def mapDecoder[T, S](implicit d: Decoder[Map[String, String Refined S => T]], v: Validate[String, S]) =
    d.emap {
      _.toList.head match {
        case (key, valueFn) => refineV[S](key).map(valueFn)
      }
    }

 def arrayCoproduct[T, S](
   implicit d: Decoder[Map[String, String Refined S => T]],
   v: Validate[String, S],
   inj: shapeless.ops.coproduct.Inject[wdl4s.cwl.Requirement,T]
   ): Decoder[Array[Requirement]] =
     mapDecoder[T, S].map(Coproduct[Requirement](_)).map(Array(_))


  def decodeCwl: Yaml => Either[Error, Cwl] =
      YamlParser.
        parse(_).
        map(_.noSpaces).
        flatMap{json =>
            println(json)
            val clt = decode[CommandLineTool](json)
            println(clt)
            clt orElse decode[Workflow](json)
        }

  //
  //This compiles
  //Decoder[ResourceRequirement]
  //This fails..
  //Decoder[RR]

}

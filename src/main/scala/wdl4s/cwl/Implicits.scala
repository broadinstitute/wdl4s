package wdl4s.cwl

import io.circe.Decoder
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

object Implicits {

  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)

  type EVR = W.`"EnvVarRequirement"`.T => EnvVarRequirement
  type IJR = W.`"InlineJavascriptRequirement"`.T => InlineJavascriptRequirement
  type SR = W.`"SoftwareRequirement"`.T => SoftwareRequirement
  type SWFR = W.`"SubworkflowFeatureRequirement"`.T => SubworkflowFeatureRequirement
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
    SWFR :+:
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
  implicit val sw = Decoder[SWFR]
  implicit def sdr = Decoder[SDR]
  implicit def rr = Decoder[RR]
  implicit def iwdr = Decoder[IWDR]
  implicit val dr = Decoder[DR]
  implicit val scr = Decoder[SCR]
  implicit val sfr = Decoder[SFR]
  implicit val mifr = Decoder[MIFR]
  implicit val sier = Decoder[SIER]

  type V[A] = ValidatedNel[String,A]

  def select[T](t: Target)(implicit selector: shapeless.ops.coproduct.Selector[Target,T]):ValidatedNel[String, T] =
             t.select[T].toValidNel(s"Expecting a EnvVarRequirement but got $t instead.")

  implicit val requirementArrayMapParser: Decoder[Array[Requirement]] =
    Decoder[Map[String, Target]].
      emap {
        m =>
          m.toList.traverse[V,Requirement] {
            case ("EnvVarRequirement", target) => select[EVR](target).map(_("EnvVarRequirement")).map(Coproduct[Requirement](_))
            case ("InlineJavascriptRequirement", target) => select[IJR](target).map(_("InlineJavascriptRequirement")).map(Coproduct[Requirement](_))
            case ("SchemaDefRequirement", target) => select[SDR](target).map(_("SchemaDefRequirement")).map(Coproduct[Requirement](_))
            case ("DockerRequirement", target) => select[DR](target).map(_("DockerRequirement")).map(Coproduct[Requirement](_))
            case ("SoftwareRequirement", target) => select[SR](target).map(_("SoftwareRequirement")).map(Coproduct[Requirement](_))
            case ("InitialWorkDirRequirement", target) => select[IWDR](target).map(_("InitialWorkDirRequirement")).map(Coproduct[Requirement](_))
            case ("ShellCommandRequirement", target) => select[SCR](target).map(_("ShellCommandRequirement")).map(Coproduct[Requirement](_))
            case ("ResourceRequirement", target) => select[RR](target).map(_("ResourceRequirement")).map(Coproduct[Requirement](_))
            case ("SubworkflowFeatureRequirement", target) => select[SWFR](target).map(_("SubworkflowFeatureRequirement")).map(Coproduct[Requirement](_))
            case ("ScatterFeatureRequirement", target) => select[SFR](target).map(_("ScatterFeatureRequirement")).map(Coproduct[Requirement](_))
            case ("MultipleInputFeatureRequirement", target) => select[MIFR](target).map(_("MultipleInputFeatureRequirement")).map(Coproduct[Requirement](_))
            case ("StepInputExpressionRequirement", target) => select[SIER](target).map(_("StepInputExpressionRequirement")).map(Coproduct[Requirement](_))
            case (key,__) => invalidNel(s"key $key was not amongst possible values " +
              "InlineJavascriptRequirement, SchemaDefRequirement, DockerRequirement, SoftwareRequirement, InitialWorkDirRequirement, EnvVarRequirement, ShellCommandRequirement, ResourceRequirement, SubworkflowFeatureRequirement, ScatterFeatureRequirement, MultipleInputFeatureRequirement, StepInputExpressionRequirement")
          }.toEither.leftMap(_.toList.mkString(", ")).map(_.toArray)
      }

}

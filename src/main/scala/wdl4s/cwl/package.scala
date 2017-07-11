package wdl4s

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
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.string._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import io.circe.Decoder.Result

import scala.util.Try
import io.circe.refined._
import io.circe.literal._

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
package object cwl {

  type CirceError[A] = Either[io.circe.Error, A]
  type CirceRead[A] = Kleisli[CirceError, String, A]

  import CwlType._
  import CwlVersion._
  import ScatterMethod._
  import RequirementClass._

  type Yaml = String

  implicit val cwlTypeDecoder = Decoder.enumDecoder(CwlType)
  implicit val cwlVersionDecoder = Decoder.enumDecoder(CwlVersion)
  implicit val scatterMethodDecoder = Decoder.enumDecoder(ScatterMethod)
  implicit val argumentClass = Decoder.enumDecoder(RequirementClass)
  implicit val scatterMethodEncoder = Encoder.enumEncoder(ScatterMethod)

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

  //This compiles
  deriveDecoder[ResourceRequirement]
  //This fails..
  deriveDecoder[RR]

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

    /*
  implicit val envvarD = deriveDecoder[EVR]
  implicit val ijr = Decoder[IJR]
  implicit val sr = Decoder[SR]
  implicit val sw = Decoder[SFT]
  implicit def sdr = deriveDecoder[SDR]
  implicit def rr = deriveDecoder[RR]
  implicit def iwdr = deriveDecoder[IWDR]
  */
  /*
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
        */
  def mapDecoder[T, S](implicit d: Decoder[Map[String, String Refined S => T]], v: Validate[String, S]) =
    d.emap {
      _.toList.head match {
        case (key, valueFn) => refineV[S](key).map(valueFn)
      }
    }
  /*
  def mapDecoder[T, S](implicit d: Decoder[(String, String Refined S => T)], v: Validate[String, S],
    imp: Lazy[DerivedDecoder[T]]) =  {
    deriveDecoder[T]
    d.emap {
      case (key, valueFn) => refineV[S](key).map(valueFn)
    }
  }


  //implicit def envVarDecoder = mapDecoder[EnvVarRequirement, MatchesRegex[W.`"EnvVarRequirement"`.T]]

  implicit def envVarDecoder: Decoder[EnvVarRequirement] = {

    val envvarDecoder2: Decoder[EnvVarRequirement] = Decoder[
      Map[String, RequirementClass => EnvVarRequirement]].
        emap {
           _.toList.head match {
             case (key, valueFn) => Try(RequirementClass.withName(key)).toEither.leftMap(_.getMessage).map(valueFn)
           }
        }

    //deriveDecoder[EnvVarRequirement] or envvarDecoder2
    envvarDecoder2
  }
  */
 def arrayCoproduct[T, S](
   implicit d: Decoder[Map[String, String Refined S => T]],
   v: Validate[String, S],
   inj: shapeless.ops.coproduct.Inject[wdl4s.cwl.Requirement,T]
   ): Decoder[Array[Requirement]] =
     mapDecoder[T, S].map(Coproduct[Requirement](_)).map(Array(_))

     /*
 implicit def ultimate(implicit rd: io.circe.generic.extras.decoding.ConfiguredDecoder[Array[wdl4s.cwl.Requirement]]
   ): Decoder[Array[Requirement]] = {
  arrayCoproduct[EnvVarRequirement, MatchesRegex[W.`"EnvVarRequirement"`.T]] or
  deriveDecoder[Array[Requirement]]
 }
 */

 /*
  implicit def p: Decoder[Array[Requirement]] = new Decoder[Array[Requirement]] {
    override def apply(c: HCursor): Result[Array[Requirement]] = ???
  }
  */

 //val f = Decoder[S :+: CNil]


 /*
 type CoproductFn = Fn :+: CNil

  implicit def envVarDecoder: Decoder[Requirement] = {

    val envvarDecoder2: Decoder[Requirement] = Decoder[
      Map[String, CoproductFn]].
        emap {
           _.toList.head match {
             case (key, valueFn) =>
               Try(RequirementClass.withName(key)).
               toEither.
               leftMap(_.getMessage).
               map(valueFn).
               map(Coproduct.apply[Requirement])
           }
        }

    //deriveDecoder[EnvVarRequirement] or envvarDecoder2
    envvarDecoder2
  }

  implicit def envVarDecoder: Decoder[EnvVarRequirement] = {

    val envvarDecoder2 = Decoder[
      Map[String, String Refined MatchesRegex[W.`"EnvVarRequirement"`.T] => EnvVarRequirement]].
        emap {
           _.toList.head match {
             case (key, valueFn) => refineV[MatchesRegex[W.`"EnvVarRequirement"`.T]](key).map(valueFn)
           }
        }

    deriveDecoder[EnvVarRequirement] or envvarDecoder2
  }
  */





  /*
  implicit def encodeAdtNoDiscr[A, Repr <: Coproduct](implicit
    gen: Generic.Aux[A, Repr],
    encodeRepr: Encoder[Repr]
  ): Encoder[A] = encodeRepr.contramap(gen.to)

  implicit def decodeAdtNoDiscr[A, Repr <: Coproduct](implicit
    gen: Generic.Aux[A, Repr],
    decodeRepr: Decoder[Repr]
): Decoder[A] = decodeRepr.map(gen.from)
        */

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

  type WorkflowStepInputId = String

  type WorkflowStepInputSource = String :+: Array[String] :+: CNil

  /**
   * These are supposed to be valid ECMAScript Expressions.
   * See http://www.commonwl.org/v1.0/Workflow.html#Expressions
   */
  type ECMAScriptExpression = String Refined MatchesRegex[W.`"$({.*}|{.*})"`.T]

  type Requirement =
    InlineJavascriptRequirement :+:
    SchemaDefRequirement :+:
    DockerRequirement :+:
    SoftwareRequirement :+:
    InitialWorkDirRequirement :+:
    EnvVarRequirement :+:
    ShellCommandRequirement :+:
    ResourceRequirement :+:
    SubworkflowFeatureRequirement :+:
    ScatterFeatureRequirement :+:
    MultipleInputFeatureRequirement :+:
    StepInputExpressionRequirement :+:
    CNil

  type MyriadInputType =
    CwlType :+:
    InputRecordSchema :+:
    InputEnumSchema :+:
    InputArraySchema :+:
    String :+:
    Array[
      CwlType :+:
      InputRecordSchema :+:
      InputEnumSchema :+:
      InputArraySchema :+:
      String :+:
      CNil
    ] :+:
    CNil

  type MyriadOutputType =
    CwlType :+:
    OutputRecordSchema :+:
    OutputEnumSchema :+:
    OutputArraySchema :+:
    String :+:
    Array[
      CwlType :+:
      OutputRecordSchema :+:
      OutputEnumSchema :+:
      OutputArraySchema :+:
      String :+:
      CNil
    ] :+:
    CNil

  type MyriadCommandInputType =
    CwlType :+:
    CommandInputRecordSchema :+:
    CommandInputEnumSchema :+:
    CommandInputArraySchema :+:
    String :+:
    Array[
      CwlType  :+:
      CommandInputRecordSchema :+:
      CommandInputEnumSchema :+:
      CommandInputArraySchema :+:
      String :+:
      CNil
      ] :+:
    CNil

    type WorkflowInput =
      Map[InputParameter#Id, InputParameter] :+:
      Map[InputParameter#Id, InputParameter#`type`] :+:
      Array[InputParameter] :+:
      CNil

    type WorkflowOutput =
      Map[WorkflowOutputParameter#Id, WorkflowOutputParameter] :+:
      Map[WorkflowOutputParameter#Id, WorkflowOutputParameter#`type`] :+:
      Array[WorkflowOutputParameter] :+:
      CNil

    type WorkflowSteps =
      Map[String, WorkflowStep] :+:
      Array[WorkflowStep] :+:
      CNil
}

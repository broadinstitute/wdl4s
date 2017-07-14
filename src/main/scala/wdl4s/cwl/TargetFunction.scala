package wdl4s.cwl

import lenthall.validation.ErrorOr.ErrorOr
import shapeless.{Witness, Coproduct}
import cats.syntax.option._

trait TargetFunction[A] {

  def apply: Target => ErrorOr[Requirement]

  protected def s2[T <: Function1[S, U], S <: String, U](s: S)(implicit
    selector: shapeless.ops.coproduct.Selector[Target,T],
    inj: shapeless.ops.coproduct.Inject[wdl4s.cwl.Requirement, U]): Target => ErrorOr[Requirement] =
      t => t.select[T].toValidNel(s"Expecting a $s but got $t instead.").map(_(s)).map(Coproduct[Requirement](_))
}

object TargetFunction {

  //"Summoner" pattern, allows for easy callup of implicit Target Function
  def apply[T]()(implicit t: TargetFunction[T]) = t

  implicit val EnvVarTargetFunction = new TargetFunction[EnvVarRequirement] {
    def apply = s2[EVR, Witness.`"EnvVarRequirement"`.T, EnvVarRequirement]("EnvVarRequirement")
  }

}


package wdl4s.wdl

trait Callable extends Scope {
  def outputs: Seq[Output]
}

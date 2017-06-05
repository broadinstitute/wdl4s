package wdl4s.wdl

trait WdlCallable extends Scope {
  def outputs: Seq[Output]
}

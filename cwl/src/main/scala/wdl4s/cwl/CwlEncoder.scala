package wdl4s.cwl

import shapeless._

object CwlEncoder extends Poly1 {
  implicit def workflow = at[Workflow]{ CwlCodecs.encodeCwlWorkflow(_) }

  implicit def commandLineTool = at[CommandLineTool]{ CwlCodecs.encodeCwlCommandLineTool(_) }
}

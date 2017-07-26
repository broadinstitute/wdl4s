package wdl4s.cwl

object CwlVersion extends Enumeration {
  type CwlVersion = Value

  val Version1 = Value("v1.0")

  val default = Version1
}

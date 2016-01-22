package wdl4s

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers, TryValues}
import wdl4s.parser.MemoryUnit

class MemorySizeSpec extends FlatSpec with Matchers with TryValues {
  "MemorySize" should "stringify properly for integer values" in {
    val memTable = Table(
      ("memorySize", "memorySizeString"),
      (MemorySize(10, MemoryUnit.Bytes), "10 B"),
      (MemorySize(10, MemoryUnit.KB), "10 KB"),
      (MemorySize(10, MemoryUnit.MB), "10 MB"),
      (MemorySize(10, MemoryUnit.GB), "10 GB"),
      (MemorySize(10, MemoryUnit.KiB), "10 KiB"),
      (MemorySize(10, MemoryUnit.MiB), "10 MiB"),
      (MemorySize(10, MemoryUnit.GiB), "10 GiB")
    )

    forAll(memTable) { (memorySize, memorySizeString) =>
      memorySize.toString shouldEqual memorySizeString
    }
  }

  it should "stringify properly for non-integer values" in {
    val memTable = Table(
      ("memorySize", "memorySizeString"),
      (MemorySize(10.5, MemoryUnit.KB), "10.5 KB"),
      (MemorySize(10.5, MemoryUnit.MB), "10.5 MB"),
      (MemorySize(10.5, MemoryUnit.GB), "10.5 GB"),
      (MemorySize(10.5, MemoryUnit.KiB), "10.5 KiB"),
      (MemorySize(10.5, MemoryUnit.MiB), "10.5 MiB"),
      (MemorySize(10.5, MemoryUnit.GiB), "10.5 GiB")
    )

    forAll(memTable) { (memorySize, memorySizeString) =>
      memorySize.toString shouldEqual memorySizeString
    }
  }

  it should "convert to bytes properly" in {
    val memTable = Table(
      ("memorySize", "bytes"),
      (MemorySize(10.5, MemoryUnit.KB), 10500.0),
      (MemorySize(10.5, MemoryUnit.MB), 10500000.0),
      (MemorySize(10.5, MemoryUnit.GB), 10500000000.0),
      (MemorySize(10.5, MemoryUnit.KiB), 10752.0),
      (MemorySize(10.5, MemoryUnit.MiB), 11010048.0),
      (MemorySize(10.5, MemoryUnit.GiB), 11274289152.0)
    )

    forAll(memTable) { (memorySize, bytes) =>
      memorySize.bytes shouldEqual bytes
    }
  }

  it should "convert to other units properly" in {
    val memTable = Table(
      ("memorySize", "convertTo", "result"),
      (MemorySize(1000, MemoryUnit.Bytes), MemoryUnit.KB, MemorySize(1, MemoryUnit.KB)),
      (MemorySize(1000000, MemoryUnit.Bytes), MemoryUnit.MB, MemorySize(1, MemoryUnit.MB)),
      (MemorySize(1024, MemoryUnit.Bytes), MemoryUnit.KiB, MemorySize(1, MemoryUnit.KiB))
    )

    forAll(memTable) { (memorySize, convertTo, result) =>
      memorySize.to(convertTo) shouldEqual result
    }
  }
}
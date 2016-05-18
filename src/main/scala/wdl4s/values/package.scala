package wdl4s

import org.apache.commons.codec.digest.DigestUtils

import scala.collection.immutable.TreeMap

package object values {

  object SymbolHash {
    def apply(clazz: Class[_ <: WdlValue], hash: String): SymbolHash = SymbolHash((clazz.getCanonicalName + hash).md5Sum)
    def apply(l: Seq[SymbolHash]): SymbolHash = apply(classOf[WdlArray], l.foldLeft("")((x, y) => x + y.value))
    def apply[K](hashedMap: Map[K, SymbolHash])(implicit ord: Ordering[K]): SymbolHash = {
      // productIterator returns an Iterator over the elements of a Tuple2 Map entry.
      val concatenatedMap = TreeMap(hashedMap.toArray: _*) flatMap { _.productIterator } mkString ""
      apply(classOf[WdlMap], concatenatedMap)
    }
  }

  case class SymbolHash(value: String) extends AnyVal with Ordered[SymbolHash] {
    def compare(that: SymbolHash) = this.value compare that.value
  }

  trait Hashable extends Any {
    def md5Sum: String
  }

  type FileHasher = WdlFile => SymbolHash

  implicit class HashableString(val value: String) extends AnyVal with Hashable {
    def md5Sum: String = DigestUtils.md5Hex(value)
    def md5SumShort: String = value.md5Sum.substring(0, 8)
  }
}

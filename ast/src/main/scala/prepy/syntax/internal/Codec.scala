package prepy.syntax.internal

object Codec {
  private val keyword = "<prepyInternalRepresentation>"
  def encode(name:    String): String  = keyword + name
  def decode(name:    String): String  = name.replaceAll(keyword, "")
  def isEncoded(name: String): Boolean = name.contains(keyword)
}

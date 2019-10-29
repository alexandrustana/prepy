package prepy.formatter

trait Formatter {
  def apply(value:  String): String
  def apply(values: List[String]): List[String] = values.map(apply)
}

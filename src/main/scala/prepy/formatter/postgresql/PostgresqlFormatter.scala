package prepy.formatter.postgresql

import prepy.formatter.Formatter

object PostgresqlFormatter extends Formatter {
  override def apply(value: String): String = snakeCaseTransformation(value)

  def snakeCaseTransformation(name: String): String = {
    val temp = name
      .foldRight("") {
        case (c, acc) => {
          if (c.isUpper) {
            if (acc.isEmpty) c.toLower else s"_${c.toLower}"
          }
          else c
        } + acc
      }
    if (temp.startsWith("_")) temp.tail
    else temp
  }
}

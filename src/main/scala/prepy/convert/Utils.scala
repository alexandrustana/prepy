package prepy.convert

trait Utils {
  type SQL = String
  type Name    = String
  type Columns = List[String]
  type Holes   = List[String]

  def structure(table: String, fields: List[Symbol]): (Name, Columns, Holes) = {
    val tableName: Name    = snakeCaseTransformation(table)
    val columns:   Columns = fields.map(_.name).map(snakeCaseTransformation)
    val holes:     Holes   = columns.map(_ => "?")

    (tableName, columns, holes)
  }

  def snakeCaseTransformation(name: String): Name = {
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

package prepy.formatter

package object postgresql {
  implicit val format: Formatter = PostgresqlFormatter
}

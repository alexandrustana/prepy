package prepy.formatter

object IdentityFormatter extends Formatter {
  override def apply(value: String): String = value
}

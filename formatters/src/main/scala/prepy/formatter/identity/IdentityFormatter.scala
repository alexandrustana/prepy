package prepy.formatter.identity

import prepy.formatter.Formatter

object IdentityFormatter extends Formatter {
  override def apply(value: String): String = value
}

package prepy.formatter

package object identity {
  implicit val format: Formatter = IdentityFormatter
}

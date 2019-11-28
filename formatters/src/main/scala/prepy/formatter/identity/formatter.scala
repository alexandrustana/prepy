package prepy.formatter

import prepy.formatter.identity.IdentityFormatter

package object formatter {
  implicit val format: Formatter = IdentityFormatter
}

package prepy.syntax.query.expection

case class InvalidQuery(message: String) extends Throwable(message) {}

package prepy.syntax.doobie

import prepy.syntax.doobie.internal.{Delete, Insert, Select, Update}

private[doobie] trait DoobieSyntax extends Select with Delete with Insert with Update {}

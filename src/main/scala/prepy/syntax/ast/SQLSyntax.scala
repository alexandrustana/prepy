package prepy.syntax.ast

import prepy.syntax.ast.internal.{Delete, Insert, Select, Update}

trait SQLSyntax extends Select with Delete with Update with Insert {}

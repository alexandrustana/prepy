package prepy.syntax.plain

import prepy.syntax.plain.operators.Operations

trait AST extends Select with Delete with Update with Insert with Operations {}

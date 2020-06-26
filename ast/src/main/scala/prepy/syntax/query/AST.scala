package prepy.syntax.query

import prepy.syntax.query.operators.Operations

trait AST extends Select with Delete with Update with Insert with Operations {}

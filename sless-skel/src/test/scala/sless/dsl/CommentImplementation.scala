package sless.dsl

import sless.ast.DSL

object CommentImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with CommentDSL with Compilable
  val dsl: DSL = DSL
}

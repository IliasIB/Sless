package sless.dsl

import sless.ast.DSL

object LessNestingImplementation {
  type DSL = PropertyDSL with NestedSelectorDSL with ValueDSL with Compilable
  val dsl: DSL = DSL
}

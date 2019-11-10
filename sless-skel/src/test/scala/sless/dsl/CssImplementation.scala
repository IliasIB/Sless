package sless.dsl

import sless.ast.DSL

object CssImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = DSL
}

package sless.dsl

object CssImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = ???
}

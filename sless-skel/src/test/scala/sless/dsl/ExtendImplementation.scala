package sless.dsl

object ExtendImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with ExtendDSL with Compilable
  val dsl: DSL = ???
}

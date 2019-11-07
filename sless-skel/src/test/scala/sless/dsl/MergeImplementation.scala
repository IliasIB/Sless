package sless.dsl

object MergeImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = ???

  def mergeSheets(cssSheets : dsl.Css*) : dsl.Css = ???
}

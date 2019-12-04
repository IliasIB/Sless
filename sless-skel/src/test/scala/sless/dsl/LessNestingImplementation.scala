package sless.dsl

import sless.ast._

object LessNestingImplementation {
  type DSL = PropertyDSL with NestedSelectorDSL with ValueDSL with Compilable
  val dsl: DSL = new NestedCompilableHandler with PropertyHandler with ValueHandler with NestedSelectorHandler {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = BaseDeclarationAST
    override type Rule = BaseRuleAST
  }
}

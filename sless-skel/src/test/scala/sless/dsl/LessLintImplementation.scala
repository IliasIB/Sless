package sless.dsl

import sless.ast._

object LessLintImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with LintDSL with Compilable
  val dsl: DSL = new CompilableHandler with SelectorHandler with PropertyHandler with LintHandler with ValueHandler {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = BaseDeclarationAST
    override type Rule = BaseRuleAST
  }
}

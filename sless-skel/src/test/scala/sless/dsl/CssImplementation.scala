package sless.dsl

import sless.ast.{CompilableHandler, CssAST, DeclarationAST, PropertyAST, PropertyHandler, RuleAST, SelectorAST, SelectorHandler, ValueAST, ValueHandler}

object CssImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = new CompilableHandler with PropertyHandler with SelectorHandler with ValueHandler {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = DeclarationAST
    override type Rule = RuleAST
  }
}

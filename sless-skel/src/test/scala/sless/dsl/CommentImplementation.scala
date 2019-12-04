package sless.dsl

import sless.ast._

object CommentImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with CommentDSL with Compilable
  val dsl: DSL = new CompilableHandler with SelectorHandler with ValueHandler with CommentHandler with PropertyHandler {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = BaseDeclarationAST
    override type Rule = BaseRuleAST
  }
}

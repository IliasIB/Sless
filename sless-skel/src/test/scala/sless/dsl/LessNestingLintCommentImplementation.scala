package sless.dsl

import sless.ast._

object LessNestingLintCommentImplementation {
  type DSL = PropertyDSL with NestedSelectorDSL with ValueDSL with LintDSL with Compilable with CommentDSL
  val dsl: DSL = new NestedCompilableHandler with NestedSelectorHandler
    with PropertyHandler with NestedLintHandler with ValueHandler with CommentHandler {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = BaseDeclarationAST
    override type Rule = BaseRuleAST
  }
}

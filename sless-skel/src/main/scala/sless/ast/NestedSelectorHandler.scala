package sless.ast

import sless.dsl.NestedSelectorDSL

trait NestedSelectorHandler extends SelectorHandler with NestedSelectorDSL {
  type RuleOrDeclaration = RuleOrDeclarationAST

  val Parent: Selector = ParentAST()

  def bindWithNesting(s: Selector, rules: Seq[RuleOrDeclaration]): Rule = {
    RuleAST(s, rules)
  }
}

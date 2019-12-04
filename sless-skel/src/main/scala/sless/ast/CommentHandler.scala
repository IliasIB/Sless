package sless.ast

import sless.dsl.CommentDSL

trait CommentHandler extends CommentDSL{
  type Declaration = BaseDeclarationAST
  type Rule = BaseRuleAST

  def commentRule(rule: Rule, str: String): Rule = {
    RuleCommentAST(rule.selector, rule.declarations, CommentAST(str))
  }
  def commentDeclaration(declaration: Declaration, str: String): Declaration = {
    DeclarationCommentAST(declaration.property, declaration.value, CommentAST(str))
  }
}

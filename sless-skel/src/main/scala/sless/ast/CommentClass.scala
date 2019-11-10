package sless.ast

import sless.dsl.CommentDSL

class CommentClass(string: String) {
  val sComment: String = string
}

trait CommentTrait extends CommentDSL{
  this: DSL.type =>

  def commentRule(rule: Rule, str: String): Rule = {
    new RuleClass(rule.sSelector, rule.sDeclarations, new CommentClass(str))
  }
  def commentDeclaration(declaration: Declaration, str: String): Declaration = {
    new DeclarationClass(declaration.sProperty, declaration.sValue, new CommentClass(str))
  }
}

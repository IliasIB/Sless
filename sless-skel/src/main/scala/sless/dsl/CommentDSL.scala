package sless.dsl

trait CommentDSL extends BaseDSL {
  protected def commentRule(rule: Rule, str: String): Rule
  protected  def commentDeclaration(declaration: Declaration, str: String): Declaration

  implicit class CommentRuleShorthand(rule : Rule) {
    def comment(string: String) : Rule = commentRule(rule, string)
  }

  implicit class CommentDeclShorthand(declaration: Declaration) {
    def comment(string: String) : Declaration = commentDeclaration(declaration, string)
  }
}

package sless.dsl

trait NestedSelectorDSL extends SelectorDSL {
  type RuleOrDeclaration
  val Parent: Selector

  override type Rule <: RuleOrDeclaration
  override type Declaration <: RuleOrDeclaration


  protected def bindWithNesting(s: Selector,
                                rules: Seq[RuleOrDeclaration]): Rule

  implicit class NestedSelectorShorthand(s: Selector)
      extends SelectorShorthand(s) {
    def nest(rules: RuleOrDeclaration*): Rule = bindWithNesting(s, rules.toSeq)
  }
}

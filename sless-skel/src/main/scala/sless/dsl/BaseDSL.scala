package sless.dsl

trait BaseDSL {
  type Rule
  type Css
  type Selector
  type Declaration
  type Property
  type Value

  def css(rules: Rule*): Css = fromRules(rules.toSeq)

  protected def fromRules(rules: Seq[Rule]): Css
}



package sless.ast

import sless.dsl.SelectorDSL


trait SelectorHandler extends SelectorDSL {
  type Selector = SelectorAST
  type Value = ValueAST
  type Rule = RuleAST
  type Declaration = DeclarationAST

  // modifiers
  def className(s: Selector, string: String): Selector = {
    ClassNameAST(s, string)
  }

  def id(s: Selector, string: String): Selector = {
    IdAST(s, string)
  }

  def attribute(s: Selector, attr: String, value: Value): Selector = {
    AttributeAST(s, attr, value)
  }

  def pseudoClass(s: Selector, string: String): Selector = {
    PseudoClassAST(s, string)
  }

  def pseudoElement(s: Selector, string: String): Selector = {
    PseudoElementAST(s, string)
  }

  // combinators
  /** -> s + selector { ... } */
  def adjacent(s: Selector, selector: Selector): Selector = {
    AdjacentAST(s, selector)
  }

  /** -> s ~ selector { ... } */
  def general(s: Selector, selector: Selector): Selector = {
    GeneralAST(s, selector)
  }

  /** -> s > selector { ... } */
  def child(s: Selector, selector: Selector): Selector = {
    ChildAST(s, selector)
  }

  /** -> s selector { ... } */
  def descendant(s: Selector, selector: Selector): Selector = {
    DescendantAST(s, selector)
  }

  // constructors
  def group(selectors: Seq[Selector]): Selector = {
    GroupAST(selectors)
  }

  def tipe(string: String): Selector = {
    TipeAST(string)
  }

  val All: Selector = AllAST()

  // bind to declarations
  def bindTo(s: Selector, declarations: Seq[Declaration]): Rule = {
    new Rule(s, declarations)
  }
}

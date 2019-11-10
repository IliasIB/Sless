package sless.ast

import sless.dsl.{MixinDSL, SelectorDSL}

class SelectorClass
case class ClassName(s: SelectorClass, string: String) extends SelectorClass
case class Id(s: SelectorClass, string: String) extends SelectorClass
case class Attribute(s: SelectorClass, attr: String, string: ValueClass) extends SelectorClass
case class PseudoClass(s: SelectorClass, string: String) extends SelectorClass
case class PseudoElement(s: SelectorClass, string: String) extends SelectorClass
case class Adjacent(s: SelectorClass, selector: SelectorClass) extends SelectorClass
case class General(s: SelectorClass, selector: SelectorClass) extends SelectorClass
case class Child(s: SelectorClass, selector: SelectorClass) extends SelectorClass
case class Descendant(s: SelectorClass, selector: SelectorClass) extends SelectorClass
case class Group(selectors: Seq[SelectorClass]) extends SelectorClass
case class Tipe(string: String) extends SelectorClass
case class All() extends SelectorClass

trait SelectorTrait extends MixinDSL {
  this: DSL.type =>

  // modifiers
  def className(s: Selector, string: String): Selector = {
    ClassName(s, string)
  }

  def id(s: Selector, string: String): Selector = {
    Id(s, string)
  }

  def attribute(s: Selector, attr: String, value: Value): Selector = {
    Attribute(s, attr, value)
  }

  def pseudoClass(s: Selector, string: String): Selector = {
    PseudoClass(s, string)
  }

  def pseudoElement(s: Selector, string: String): Selector = {
    PseudoElement(s, string)
  }

  // combinators
  /** -> s + selector { ... } */
  def adjacent(s: Selector, selector: Selector): Selector = {
    Adjacent(s, selector)
  }

  /** -> s ~ selector { ... } */
  def general(s: Selector, selector: Selector): Selector = {
    General(s, selector)
  }

  /** -> s > selector { ... } */
  def child(s: Selector, selector: Selector): Selector = {
    Child(s, selector)
  }

  /** -> s selector { ... } */
  def descendant(s: Selector, selector: Selector): Selector = {
    Descendant(s, selector)
  }

  // constructors
  def group(selectors: Seq[Selector]): Selector = {
    Group(selectors)
  }

  def tipe(string: String): Selector = {
    Tipe(string)
  }

  // bind to declarations
  def bindTo(s: Selector, declarations: Seq[Declaration]): Rule = {
    new RuleClass(s, declarations)
  }

  val All: Selector = new All()
}

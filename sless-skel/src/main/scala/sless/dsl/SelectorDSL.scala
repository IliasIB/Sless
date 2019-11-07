package sless.dsl

/**
  * https://developer.mozilla.org/en-US/docs/Web/CSS/Reference#Selectors
  */
trait SelectorDSL extends BaseDSL {
  // modifiers
  protected def className(s: Selector, string: String): Selector
  protected def id(s: Selector, string: String): Selector
  protected def attribute(s: Selector, attr: String, value: Value): Selector

  protected def pseudoClass(s: Selector, string: String): Selector
  protected def pseudoElement(s: Selector, string: String): Selector

  // combinators
  /** -> s + selector { ... } */
  protected def adjacent(s: Selector, selector: Selector): Selector

  /** -> s ~ selector { ... } */
  protected def general(s: Selector, selector: Selector): Selector

  /** -> s > selector { ... } */
  protected def child(s: Selector, selector: Selector): Selector

  /** -> s selector { ... } */
  protected def descendant(s: Selector, selector: Selector): Selector

  // constructors
  protected def group(selectors: Seq[Selector]): Selector
  def tipe(string: String): Selector
  val All: Selector
  def N(selectors: Selector*): Selector = group(selectors.toSeq)

  // bind to declarations
  protected def bindTo(s: Selector, declarations: Seq[Declaration]): Rule

  // shorthand
  implicit class SelectorShorthand(s: Selector) {
    def c(string: String): Selector = className(s, string)
    def ##(string: String): Selector = id(s, string)
    def at(attr: String, value: Value): Selector = attribute(s, attr, value)

    def :|(pseudo: String): Selector = pseudoClass(s, pseudo)
    def ::(pseudoEl: String): Selector = pseudoElement(s, pseudoEl)

    def |~(selector: Selector): Selector = adjacent(s, selector)
    def |+(selector: Selector): Selector = general(s, selector)
    def |>(selector: Selector): Selector = child(s, selector)
    def |-(selector: Selector): Selector = descendant(s, selector)

    def apply(declarations: Declaration*): Rule = bindTo(s, declarations.toSeq)
  }
}

package sless.ast

// Base classes
case class CssAST(rules: Seq[RuleAST])
case class RuleAST(selector: SelectorAST, declarations: Seq[DeclarationAST])
case class DeclarationAST(property: PropertyAST, value: ValueAST)
case class PropertyAST(property: String)
case class ValueAST(value: String)

// Selector classes
class SelectorAST()
case class ClassNameAST(s: SelectorAST, string: String) extends SelectorAST
case class IdAST(s: SelectorAST, string: String) extends SelectorAST
case class AttributeAST(s: SelectorAST, attr: String, string: ValueAST) extends SelectorAST
case class PseudoClassAST(s: SelectorAST, string: String) extends SelectorAST
case class PseudoElementAST(s: SelectorAST, string: String) extends SelectorAST
case class AdjacentAST(s: SelectorAST, SelectorAST: SelectorAST) extends SelectorAST
case class GeneralAST(s: SelectorAST, SelectorAST: SelectorAST) extends SelectorAST
case class ChildAST(s: SelectorAST, SelectorAST: SelectorAST) extends SelectorAST
case class DescendantAST(s: SelectorAST, SelectorAST: SelectorAST) extends SelectorAST
case class GroupAST(SelectorASTs: Seq[SelectorAST]) extends SelectorAST
case class TipeAST(string: String) extends SelectorAST
case class AllAST() extends SelectorAST

package sless.ast

// Base
// Base classes
trait BaseRuleAST extends RuleOrDeclarationAST {
  def selector: SelectorAST
  def declarations: Seq[RuleOrDeclarationAST]
}
trait BaseDeclarationAST extends RuleOrDeclarationAST  {
  def property: PropertyAST
  def value: ValueAST
}
case class CssAST(rules: Seq[BaseRuleAST])
case class RuleAST(selector: SelectorAST, declarations: Seq[RuleOrDeclarationAST]) extends BaseRuleAST
case class DeclarationAST(property: PropertyAST, value: ValueAST) extends BaseDeclarationAST
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

// Extensions
// Comment implementation classes
case class CommentAST(comment: String)
case class RuleCommentAST(selector: SelectorAST, declarations: Seq[RuleOrDeclarationAST], comment: CommentAST)
  extends BaseRuleAST
case class DeclarationCommentAST(property: PropertyAST, value: ValueAST, comment: CommentAST)
  extends BaseDeclarationAST

// Nested classes
trait RuleOrDeclarationAST
case class ParentException(message: String = "Root element can't have Parent") extends Exception(message)
case class ParentAST() extends SelectorAST

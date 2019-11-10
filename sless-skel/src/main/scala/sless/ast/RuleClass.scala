package sless.ast

class RuleClass(selector: SelectorClass, declarations: Seq[DeclarationClass],
                comment: CommentClass = new CommentClass("")) {
  val sSelector: SelectorClass = selector
  val sDeclarations: Seq[DeclarationClass] = declarations
  val sComment: CommentClass = comment
}

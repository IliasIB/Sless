package sless.ast

case class RuleClass(sSelector: SelectorClass, sDeclarations: Seq[RuleOrDeclarationClass],
                     sComment: CommentClass = new CommentClass("")) extends RuleOrDeclarationClass
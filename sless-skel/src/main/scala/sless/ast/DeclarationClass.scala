package sless.ast

case class DeclarationClass(sProperty: PropertyClass, sValue: ValueClass,
                            sComment: CommentClass = new CommentClass("")) extends RuleOrDeclarationClass
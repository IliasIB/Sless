package sless.ast

class DeclarationClass(property: PropertyClass, value: ValueClass,
                       comment: CommentClass = new CommentClass("")) {
  val sProperty: PropertyClass = property
  val sValue: ValueClass = value
  val sComment: CommentClass = comment
}

package sless.ast

import sless.dsl.PropertyDSL

class PropertyClass(property: String) {
  val sProperty: String = property
}

trait PropertyTrait extends PropertyDSL {
  this: DSL.type =>

  def assign(p: Property, value: Value): Declaration = {
    new DeclarationClass(p, value)
  }
  def prop(string: String): Property = {
    new PropertyClass(string)
  }
}

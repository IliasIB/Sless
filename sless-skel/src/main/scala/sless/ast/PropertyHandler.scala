package sless.ast

import sless.dsl.PropertyDSL

trait PropertyHandler extends PropertyDSL{
  type Declaration = DeclarationAST
  type Property = PropertyAST
  type Value = ValueAST

  def assign(p: Property, value: Value): Declaration = {
    new Declaration(p, value)
  }
  def prop(string: String): Property = {
    new Property(string)
  }
}

package sless.ast

import sless.dsl.PropertyDSL

trait PropertyHandler extends PropertyDSL{
  type Declaration = BaseDeclarationAST
  type Property = PropertyAST
  type Value = ValueAST

  def assign(p: Property, value: Value): Declaration = {
    DeclarationAST(p, value)
  }
  def prop(string: String): Property = {
    PropertyAST(string)
  }
}

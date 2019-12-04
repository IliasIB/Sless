package sless.ast

import sless.dsl.ValueDSL

trait ValueHandler extends ValueDSL{
  type Value = ValueAST
  def value(string: String): Value = {
    ValueAST(string)
  }
}

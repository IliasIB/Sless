package sless.ast

import sless.dsl.ValueDSL

class ValueClass(value: String) {
  val sValue: String = value
}
trait ValueTrait extends ValueDSL {
  this: DSL.type =>
  def value(string: String): Value = {
    new ValueClass(string)
  }
}

package sless.dsl

trait ValueDSL extends BaseDSL {
  def value(string: String): Value
}

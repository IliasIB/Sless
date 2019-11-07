package sless.dsl

trait Compilable { self: BaseDSL =>
  def compile(sheet: self.Css): String
  def pretty(sheet: self.Css, spaces: Int): String
}

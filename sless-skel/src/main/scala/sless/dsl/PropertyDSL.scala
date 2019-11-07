package sless.dsl

trait PropertyDSL extends BaseDSL {
  def prop(string: String): Property

  protected def assign(p: Property, value: Value): Declaration

  implicit class DeclarationShorthand(d: Property) {
    def :=(value: Value): Declaration = assign(d, value)
  }
}

package sless.dsl

trait ExtendDSL extends BaseDSL {

  protected def extendI(s : Selector,selector: Selector) : Selector

  implicit class ExtendShorthand(s: Selector) {
    def extend(selector: Selector): Selector ={
      extendI(s,selector)
    }
  }

}

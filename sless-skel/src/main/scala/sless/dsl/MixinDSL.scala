package sless.dsl

trait MixinDSL extends BaseDSL with SelectorDSL {

  implicit class MixinShorthand(s: Selector) {

    def mixin(d1: Seq[Declaration]*): Rule = bindTo(s, d1.flatten)

  }
}

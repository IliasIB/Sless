package sless.dsl

import sless.ast._

object MixinImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with MixinDSL with Compilable
  val dsl: DSL = new CompilableHandler with PropertyHandler with SelectorHandler with ValueHandler with MixinDSL {
    override type Value = ValueAST
    override type Selector = SelectorAST
    override type Property = PropertyAST
    override type Css = CssAST
    override type Declaration = BaseDeclarationAST
    override type Rule = BaseRuleAST
  }

  import dsl._

  /**
    * Create a non-parametric mixin that contains the declarations:
    *     color: red;
    *     background: white;
    */
  val simpleColorMixin : Seq[dsl.Declaration] = List(
    prop("color") := value("red"),
    prop("background") := value("white")
  )

  /**
    * Create a parametric mixin that takes a $mixin and an argument $padding as arguments, and consists of the following declarations:
    *     $mixin **i.e., mixin's declarations come first**
    *     margin: $padding;
    */
  val nestedMixin : (Seq[dsl.Declaration], String) => Seq[dsl.Declaration] = (mixin: Seq[dsl.Declaration], padding: String) =>
    mixin.toList.:+(prop("margin") := value(padding))

  /**
    * NOTE: Remember this assignment? Without realizing it, you already implemented a kind of parametric mixin earlier while experimenting with variables,
    * but we did not have convenient mixin application syntax back then. Reimplement your mixin here, but as a mixin value.
    *
    * Create a sequence of declaration for the given height that results in an aspect ratio of 2/1
    *     height: $height;
    *     width: $height * 2;
    */
   val doubledWidthMixin : Int => Seq[dsl.Declaration] = (height: Int) => Seq(
     prop("height") := value(height.toString),
     prop("width") := value((height * 2).toString)
   )

}

package sless.dsl

import org.scalatest.FunSuite

class MixinTest extends FunSuite {
  import MixinImplementation.dsl._
  import MixinImplementation.{simpleColorMixin, nestedMixin, doubledWidthMixin}


  test("Simple mixin") {
    val asgn = prop("property-1") := value("value-1")
    val mixin = List(asgn,asgn)
    val propertiesPre = List(prop("property-pre") := value("value-pre"))
    val propertiesPost = List(prop("property-post") := value("value-post"))
    val rule = All.mixin(propertiesPre, mixin, propertiesPost)
    val ex = css(rule)

    assert(
      MixinImplementation.dsl.compile(ex) ===
        """*{property-pre:value-pre;property-1:value-1;property-1:value-1;property-post:value-post;}""")
  }

  test("Parametric mixin") {
    val asgnPar : String => Declaration = x  => prop(x) := value("value-1")
    val mixin : String => Seq[Declaration] = x  =>  List(asgnPar(x))
    val propertiesPre = List(prop("property-pre") := value("value-pre"))
    val propertiesPost = List(prop("property-post") := value("value-post"))
    val rule = All.mixin(propertiesPre,mixin("Hey-Im-a-mixin"), propertiesPost)
    val ex = css(rule)

    assert(
      MixinImplementation.dsl.compile(ex) ===
        """*{property-pre:value-pre;Hey-Im-a-mixin:value-1;property-post:value-post;}""")
  }

  test("Nested, double width, color mixin") {
    val propertiesPre = List(prop("property-pre") := value("value-pre"))
    val rule = All.mixin(propertiesPre, nestedMixin(simpleColorMixin, "50"), doubledWidthMixin(5))
    val ex = css(rule)

    assert(
      MixinImplementation.dsl.compile(ex) ===
      """*{property-pre:value-pre;color:red;background:white;margin:50;height:5;width:10;}"""
    )
  }
}

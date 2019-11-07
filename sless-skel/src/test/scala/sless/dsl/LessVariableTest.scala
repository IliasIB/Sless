package sless.dsl

import org.scalatest.FunSuite

class LessVariableTest extends FunSuite {
  import LessVariableImplementation.dsl._

  test("Red color test") {
    val ex = LessVariableImplementation.colorNamedRed("testId")

    assert(
      LessVariableImplementation.dsl.compile(ex) ===
        """*#testId{background-color:red;}""")
  }

  test("Test width") {
    val ex = css(LessVariableImplementation.doubledWidth("testId",20))

    assert(
      LessVariableImplementation.dsl.compile(ex) ===
        """testId{height:20;width:40;}""")
  }

}

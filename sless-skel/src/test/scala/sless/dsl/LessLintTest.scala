package sless.dsl

import org.scalatest.FunSuite

class LessLintTest extends FunSuite{
  import LessLintImplementation.dsl._

  test("Remove empty rules") {
    val container = tipe("div") ## "container"

    val ex = css(
      N(All.c("class-name1"), All.c("class-name2"))(),
      container {
        prop("width") := value("100%")
      },
    )

   val (lintedBool, lintedEx) = removeEmptyRules(ex)
    assert(lintedBool === true)
    assert(
      LessLintImplementation.dsl.compile(lintedEx) ===
        """div#container{width:100%;}""")
  }

  test("Aggregate margins") {
    val container = tipe("div") ## "container"

    val ex = css(
      container (
        prop("margin-right")  := value("50px"),
        prop("margin-bottom")  := value("75px"),
        prop("width") := value("100%"),
        prop("margin-top")  := value("25px"),
        prop("margin-left")  := value("100px")
      )
    )

    val (lintedBool, lintedEx) = aggregateMargins(ex)
    assert(lintedBool === true)
    assert(
      LessLintImplementation.dsl.compile(lintedEx) ===
        """div#container{margin:25px 50px 75px 100px;width:100%;}""")
  }

  test("Limit Floats") {
    val container = tipe("div") ## "container"
    val fldecl = prop("float") := value("left")

    val ex1 = css(
      N(All.c("class-name1"), All.c("class-name2"))(fldecl),
      container(fldecl),
    )
    val ex2 = css(container(fldecl))

    val lintedBool1 = limitFloats(ex1,1)
    assert(lintedBool1 === true)

    val lintedBool2 = limitFloats(ex2,1)
    assert(lintedBool2 === false)
  }
}

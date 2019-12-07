package sless.dsl

import org.scalatest.FunSuite

class LessNestingLintCommentTest extends FunSuite{
  import LessNestingLintCommentImplementation.dsl._

  test("Remove nested empty rules") {
    val nestedExample = css(
      (All ## "header").nest(
        prop("color") := value("black"),
        All.c("logo")()
      )
    )

    val (lintedBool, lintedEx) = removeEmptyRules(nestedExample)
    assert(lintedBool === true)
    assert(
      LessNestingLintCommentImplementation.dsl.compile(lintedEx) ===
        """*#header{color:black;}""")
  }

  test("Aggregate margins") {
    val nestedExample = css(
      (All ## "header").nest(
        prop("color") := value("black"),
        All.c("logo")(
          prop("margin-right")  := value("50px"),
          prop("margin-bottom")  := value("75px"),
          prop("width") := value("100%"),
          prop("margin-top")  := value("25px"),
          prop("margin-left")  := value("100px")
        )
      )
    )

    val (lintedBool, lintedEx) = aggregateMargins(nestedExample)
    assert(lintedBool === true)
    assert(
      LessNestingLintCommentImplementation.dsl.compile(lintedEx) ===
        """*#header{color:black;}*#header *.logo{margin:25px 50px 75px 100px;width:100%;}""")
  }

  test("Limit Floats") {
    val nestedExample = css(
      (All ## "header").nest(
        prop("float") := value("right"),
        All.c("logo")(prop("float") := value("left"))
      )
    )
    val lintedBool1 = limitFloats(nestedExample,1)
    assert(lintedBool1 === true)
  }

  test("Pretty print nested with comments") {
    val div = tipe("div")
    val li = tipe("li")
    val ul = tipe("ul")
    val display = prop("display")
    val block = value("block")
    val parentExample = css(
      (All ## "header").nest(
        (Parent |+ Parent).nest(
          (Parent |- Parent)(prop("width") := value("300px"))
        )
      ) ,
      (div.c("menu-bar") |- li :| "hover" |> ul) {
        display := block
      }.comment("This is a comment")
    )

    assert(
      LessNestingLintCommentImplementation.dsl.pretty(parentExample, 4) ===
        """*#header + *#header *#header + *#header {
    width: 300px;
}

/* This is a comment */
div.menu-bar li:hover > ul {
    display: block;
}""")
  }
}

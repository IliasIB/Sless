package sless.dsl

import org.scalatest.FunSuite

class CommentTest extends FunSuite {
  import CommentImplementation.dsl._

  test("CSS with comments") {
    val backgroundColor = prop("background-color")
    val container = tipe("div") ## "container"

    val ex = css(
      (N(All.c("class-name1"), All.c("class-name2")) {
        prop("width") := value("100%")
      }).comment( "something with class 1 and 2"),
      container {
        (backgroundColor := value("blue")).comment("bg is blue")
      }
    )

    assert(
      compile(ex) ===
        """/* something with class 1 and 2 */*.class-name1,*.class-name2{width:100%;}div#container{background-color:blue;/* bg is blue */}""")


    assert(
      pretty(ex, 4) ===
        """/* something with class 1 and 2 */
*.class-name1, *.class-name2 {
    width: 100%;
}

div#container {
    background-color: blue; /* bg is blue */
}""")
  }

  test("Pretty print nested with comments") {
    val ex = css(
      (All ## "header").nest(
        (Parent |+ Parent).nest(
          (Parent |- Parent)(prop("width") := value("300px"))
        )
      ).comment("Nested")
    )
    assert(
      pretty(ex, 4) ===
        """/* Nested */
*#header *#header + *#header *#header *#header + *#header {
    width: 300px;
}""")
  }
}

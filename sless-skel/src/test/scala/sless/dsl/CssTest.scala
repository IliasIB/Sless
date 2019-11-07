package sless.dsl

import org.scalatest.FunSuite

class CssTest extends FunSuite {
  import CssImplementation.dsl._

  test("Simple CSS") {
    val backgroundColor = prop("background-color")
    val container = tipe("div") ## "container"

    val ex = css(
      N(All.c("class-name1"), All.c("class-name2")) {
        prop("width") := value("100%")
      },
      container {
        backgroundColor := value("blue")
      },
      (container :| "hover") {
        backgroundColor := value("red")
      }
    )

    assert(
      CssImplementation.dsl.compile(ex) ===
        """*.class-name1,*.class-name2{width:100%;}div#container{background-color:blue;}div#container:hover{background-color:red;}""")

    assert(
      CssImplementation.dsl.pretty(ex, 4) ===
        """*.class-name1, *.class-name2 {
    width: 100%;
}

div#container {
    background-color: blue;
}

div#container:hover {
    background-color: red;
}""")
  }

  test("Assignment CSS") {
    val strong = tipe("strong")
    val color = prop("color")
    val red = value("red")

    val div = tipe("div")
    val li = tipe("li")
    val ul = tipe("ul")
    val display = prop("display")
    val block = value("block")

    val ex = css(
     strong {
       color := red
     } ,
      (div.c("menu-bar") |- li :| "hover" |> ul) {
        display := block
      }
    )

    assert(
      CssImplementation.dsl.compile(ex) ===
        """strong{color:red;}div.menu-bar li:hover>ul{display:block;}""")

    assert(
      CssImplementation.dsl.pretty(ex, 0) ===
        """strong {
color: red;
}

div.menu-bar li:hover > ul {
display: block;
}""")
  }
}

package sless.dsl

import sless.ast.{DSL}

/**
  * All Lint checks you implement in this file are of a similar form: they accept a CSS sheet,
  * check a particular boolean condition, and return a pair consisting of a Boolean and a fixed sheet
  *  (only in case the boolean is true, otherwise the original sheet is returned).
  *  A sheet is only returned if the code mistake can be fixed automatically.
  * */

object LessLintImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = DSL
  import sless.ast._


  /**
    * Check if the given sheet has any style rules without declarations, i.e. of the form "selector {}"
    */
  def removeEmptyRules(css : dsl.Css) : (Boolean, dsl.Css) = {
    val tempCss: CssClass = css.asInstanceOf[CssClass]
    val newRules = tempCss.sRules.filter(rule => (rule.sDeclarations.nonEmpty)).asInstanceOf[Seq[dsl.Rule]]
    if (newRules.size < tempCss.sRules.size){
      (true, dsl.css(newRules: _*))
    } else {
      (false, css)
    }
  }

  /**
    * Check if the given sheet has any style rules with a  declaration for all four properties from the set
    * margin-left, margin-right, margin-top, and margin-bottom, and if so, replaces each property by
    * the single shorthand property margin. The new margin property takes the place of the first declaration in order of appearance.
    * The values from the individual prorperties are aggregated in the order top-right-bottom-left, with spaces in between.
    */
  def aggregateMargins(css : dsl.Css) : (Boolean, dsl.Css) = {
    val tempCss: CssClass = css.asInstanceOf[CssClass]
    def isInRule(rule: RuleClass, string: String): Boolean = {
      if (rule.sDeclarations.exists(declaration => declaration.sProperty.sProperty == string)) {
        true
      } else {
        false
      }
    }

    val delNeeded = tempCss.sRules.exists((rule) => (
      isInRule(rule, "margin-left") && isInRule(rule, "margin-right") &&
        isInRule(rule, "margin-top") && isInRule(rule, "margin-bottom")))

    val cleanedRules = tempCss.sRules.map((rule) => (
        if (delNeeded) {
          val topMargin = rule.sDeclarations.find(declaration =>
            "margin-top" == declaration.sProperty.sProperty).get.sValue.sValue
          val leftMargin = rule.sDeclarations.find(declaration =>
            "margin-left" == declaration.sProperty.sProperty).get.sValue.sValue
          val rightMargin = rule.sDeclarations.find(declaration =>
            "margin-right" == declaration.sProperty.sProperty).get.sValue.sValue
          val bottomMargin = rule.sDeclarations.find(declaration =>
            "margin-bottom" == declaration.sProperty.sProperty).get.sValue.sValue
          val newDeclaration = new DeclarationClass(new PropertyClass("margin"),
            new ValueClass(List(topMargin, rightMargin, bottomMargin, leftMargin).mkString(" ")))
          val tempDeclarations = rule.sDeclarations.filter(declaration =>
            declaration.sProperty.sProperty match {
              case "margin-left" | "margin-right" |"margin-top" |"margin-bottom" => false
              case _ => true
            }
          )
          val newDeclarations = tempDeclarations.toList.::(newDeclaration)
          new RuleClass(rule.sSelector, newDeclarations)
        } else {
          rule
        }
      ))
    (delNeeded, dsl.css(cleanedRules.asInstanceOf[Seq[dsl.Rule]]: _*))
  }

  /**
    * Check if the given sheet contains strictly more than n 'float' properties and, if so, returns true, otherwise false.
    *
    */
  def limitFloats(css : dsl.Css, n : Integer) : Boolean  = {
    val tempCss: CssClass = css.asInstanceOf[CssClass]
    val floatsFound = tempCss.sRules.count((rule) => (
      rule.sDeclarations.exists((declaration) => (declaration.sProperty.sProperty == "float"))))
    if (floatsFound > n){
      true
    } else {
      false
    }
  }

}

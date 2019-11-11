package sless.dsl

import sless.ast.{DSL}

/**
  * All Lint checks you implement in this file are of a similar form: they accept a CSS sheet,
  * check a particular boolean condition, and return a pair consisting of a Boolean and a fixed sheet
  *  (only in case the boolean is true, otherwise the original sheet is returned).
  *  A sheet is only returned if the code mistake can be fixed automatically.
  * */

object LessLintImplementation {
  type DSL = PropertyDSL with NestedSelectorDSL with ValueDSL with Compilable
  val dsl: DSL = DSL
  import sless.ast._


  /**
    * Check if the given sheet has any style rules without declarations, i.e. of the form "selector {}"
    */
  def removeEmptyRules(css : dsl.Css) : (Boolean, dsl.Css) = {
    val tempCss: CssClass = css.asInstanceOf[CssClass]
    val tempRules = tempCss.sRules.filter(rule => (rule.sDeclarations.nonEmpty)).asInstanceOf[Seq[dsl.Rule]]
    val newTuples = tempRules.map(rule => removeEmptyRules(rule.asInstanceOf[RuleClass]))
    val delWasNeeded = newTuples.exists(tuple => tuple._1)
    val newRules = newTuples.map(tuple => tuple._2).asInstanceOf[Seq[dsl.Rule]]
    if (newRules.size < tempCss.sRules.size || delWasNeeded){
      (true, dsl.css(newRules: _*))
    } else {
      (false, css)
    }
  }
  def removeEmptyRules(rule: RuleClass) : (Boolean, RuleClass) = {
    val rulesAndDeclarations = rule.sDeclarations.partition {
      case RuleClass(_, _, _) => true
      case _ => false
    }
    val rules = rulesAndDeclarations._1.asInstanceOf[Seq[RuleClass]]
    val declarations = rulesAndDeclarations._2.asInstanceOf[Seq[DeclarationClass]]
    if (rules.nonEmpty) {
      val tempRules = rules.filter(rule => (rule.sDeclarations.nonEmpty)).asInstanceOf[Seq[dsl.Rule]]
      val newTuples = tempRules.map(rule => removeEmptyRules(rule.asInstanceOf[RuleClass]))
      val delWasNeeded = newTuples.exists(tuple => tuple._1)
      val newRule = RuleClass(rule.sSelector, declarations.asInstanceOf[Seq[RuleOrDeclarationClass]]
        .++(rules.asInstanceOf[Seq[RuleOrDeclarationClass]]), rule.sComment)
      (delWasNeeded, newRule)
    }
    else {
      (false, rule)
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

    val cleanedTuples = tempCss.sRules.map(rule => cleanRule(rule))
    val delWasNeeded = cleanedTuples.exists(tuple => tuple._1)
    val cleanedRules = cleanedTuples.map(tuple => tuple._2)
    if (delWasNeeded)
      (true, dsl.css(cleanedRules.asInstanceOf[Seq[dsl.Rule]]: _*))
    else
      (false, css)
  }

  def isInRule(rule: RuleClass, string: String): Boolean = {
    val declarations = rule.sDeclarations.filter {
      case DeclarationClass(_, _, _) => true
      case _ => false
    }
    if (declarations.exists(declaration => declaration.asInstanceOf[DeclarationClass].sProperty.sProperty == string)) {
      true
    } else {
      false
    }
  }

  def cleanRule(rule: RuleClass): (Boolean, RuleClass) = {
    val declarationsAndRules = rule.sDeclarations.partition {
      case DeclarationClass(_, _, _) => true
      case _ => false
    }
    val declarations = declarationsAndRules._1.asInstanceOf[Seq[DeclarationClass]]
    val rules = declarationsAndRules._2.asInstanceOf[Seq[RuleClass]]

    if (isInRule(rule, "margin-left") && isInRule(rule, "margin-right") &&
      isInRule(rule, "margin-top") && isInRule(rule, "margin-bottom")) {
      val topMargin = declarations.find(declaration =>
        "margin-top" == declaration.sProperty.sProperty).get.sValue.sValue
      val leftMargin = declarations.find(declaration =>
        "margin-left" == declaration.sProperty.sProperty).get.sValue.sValue
      val rightMargin = declarations.find(declaration =>
        "margin-right" == declaration.sProperty.sProperty).get.sValue.sValue
      val bottomMargin = declarations.find(declaration =>
        "margin-bottom" == declaration.sProperty.sProperty).get.sValue.sValue
      val newDeclaration = DeclarationClass(new PropertyClass("margin"),
        new ValueClass(List(topMargin, rightMargin, bottomMargin, leftMargin).mkString(" ")))
      val tempDeclarations = declarations.filter(declaration =>
        declaration.sProperty.sProperty match {
          case "margin-left" | "margin-right" |"margin-top" |"margin-bottom" => false
          case _ => true
        }
      )
      if (rules.nonEmpty) {
        val cleanedRules = rules.map(rule => cleanRule(rule)._2)
        val newDeclarations = tempDeclarations.toList.::(newDeclaration).:+(cleanedRules).asInstanceOf[Seq[RuleOrDeclarationClass]]
        (true, RuleClass(rule.sSelector, newDeclarations))
      } else {
        val newDeclarations = tempDeclarations.toList.::(newDeclaration)
        (true, RuleClass(rule.sSelector, newDeclarations))
      }
    } else {
      if (rules.nonEmpty) {
        val cleanedTuples = rules.map(rule => cleanRule(rule))
        val delWasNeeded = cleanedTuples.exists(tuple => tuple._1)
        val cleanedRules = cleanedTuples.map(tuple => tuple._2).toList
        val newDeclarations = declarations.toList.:::(cleanedRules).asInstanceOf[Seq[RuleOrDeclarationClass]]
        (delWasNeeded, RuleClass(rule.sSelector, newDeclarations))
      } else {
        (false, rule)
      }
    }
  }

  /**
    * Check if the given sheet contains strictly more than n 'float' properties and, if so, returns true, otherwise false.
    *
    */
  def limitFloats(css : dsl.Css, n : Integer) : Boolean  = {
    val tempCss: CssClass = css.asInstanceOf[CssClass]
    val floatsFound = tempCss.sRules.map(rule => countFloats(rule)).sum
    if (floatsFound > n){
      true
    } else {
      false
    }
  }
  def countFloats(rule: RuleClass): Int = {
    val declarationsAndRules = rule.sDeclarations.partition {
      case DeclarationClass(_, _, _) => true
      case _ => false
    }
    val floatsFound = declarationsAndRules._1.count(declaration =>
      declaration.asInstanceOf[DeclarationClass].sProperty.sProperty == "float")
    val floatsInNested = declarationsAndRules._2.map(rule => countFloats(rule.asInstanceOf[RuleClass])).sum
    floatsFound + floatsInNested
  }

}

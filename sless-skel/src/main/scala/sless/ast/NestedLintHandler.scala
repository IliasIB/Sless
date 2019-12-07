package sless.ast

import sless.dsl.LintDSL

trait NestedLintHandler extends LintDSL{
  type Css = CssAST

  def removeEmptyRules(css : Css) : (Boolean, Css) = {
    val newTuples = css.rules.filter(rule => rule.declarations.nonEmpty).map(rule => removeEmptyRules(rule))
    val newRules = newTuples.map(tuple => tuple._2)
    if (newRules.size < css.rules.size || newTuples.exists(tuple => tuple._1)){
      (true, CssAST(newRules))
    } else {
      (false, css)
    }
  }

  def removeEmptyRules(rule: BaseRuleAST) : (Boolean, BaseRuleAST) = {
    val rulesAndDeclarations = rule.declarations.partition {
      case _:  BaseRuleAST => true
      case _: BaseDeclarationAST => false
    }
    val rules = rulesAndDeclarations._1.asInstanceOf[Seq[BaseRuleAST]]
    val declarations = rulesAndDeclarations._2.asInstanceOf[Seq[BaseDeclarationAST]]
    if (rules.nonEmpty) {
      val newTuples = rules.filter(rule => rule.declarations.nonEmpty).map(rule => removeEmptyRules(rule))
      val cleanedRules = newTuples.map(tuple => tuple._2)
      val newRule = rule match {
        case RuleAST(selector, _) => RuleAST(selector, declarations ++ cleanedRules)
        case RuleCommentAST(selector, _, comment) => RuleCommentAST(selector, declarations ++ cleanedRules, comment)
      }
      (newTuples.exists(tuple => tuple._1) || cleanedRules.isEmpty, newRule)
    } else {
      (false, rule)
    }
  }

  def aggregateMargins(css : Css) : (Boolean, Css) = {
    val cleanedTuples = css.rules.map(rule => cleanRule(rule))
    val delWasNeeded = cleanedTuples.exists(tuple => tuple._1)
    val cleanedRules = cleanedTuples.map(tuple => tuple._2)
    if (delWasNeeded)
      (true, (new NestedCompilableHandler).css(cleanedRules.asInstanceOf[Seq[BaseRuleAST]]: _*))
    else
      (false, css)
  }

  private def getValueOfPropertyInRule(rule: BaseRuleAST, property: String): String = {
    rule.declarations.find(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == property)
      .get.asInstanceOf[BaseDeclarationAST].value.value
  }

  private def isPropertyInRule(rule: BaseRuleAST, property: String): Boolean = {
    rule.declarations.filter(rule => rule.isInstanceOf[BaseDeclarationAST])
      .exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == property)
  }

  def cleanRule(rule: BaseRuleAST): (Boolean, BaseRuleAST) = {
    val declarationsAndRules = rule.declarations.partition {
      case _: BaseDeclarationAST => true
      case _ => false
    }
    val declarations = declarationsAndRules._1.asInstanceOf[Seq[BaseDeclarationAST]]
    val rules = declarationsAndRules._2.asInstanceOf[Seq[BaseRuleAST]]

    if (isPropertyInRule(rule, "margin-left") && isPropertyInRule(rule, "margin-right") &&
      isPropertyInRule(rule, "margin-top") && isPropertyInRule(rule, "margin-bottom")) {
      val newDeclaration = DeclarationAST(PropertyAST("margin"), ValueAST(
        getValueOfPropertyInRule(rule, "margin-top") + " " + getValueOfPropertyInRule(rule, "margin-right") + " " +
          getValueOfPropertyInRule(rule, "margin-bottom") + " " + getValueOfPropertyInRule(rule, "margin-left")
      ))
      val tempDeclarations = declarations.filter(declaration =>
        declaration.property.property match {
          case "margin-left" | "margin-right" |"margin-top" |"margin-bottom" => false
          case _ => true
        }
      )
      if (rules.nonEmpty) {
        val cleanedRules = rules.map(rule => cleanRule(rule)._2)
        val newDeclarations = tempDeclarations.toList.::(newDeclaration).:+(cleanedRules).asInstanceOf[Seq[RuleOrDeclarationAST]]
        (true, RuleAST(rule.selector, newDeclarations))
      } else {
        val newDeclarations = tempDeclarations.toList.::(newDeclaration)
        (true, RuleAST(rule.selector, newDeclarations))
      }

    } else {
      if (rules.nonEmpty) {
        val cleanedTuples = rules.map(rule => cleanRule(rule))
        val cleanedRules = cleanedTuples.map(tuple => tuple._2).toList
        val newDeclarations = declarations.toList.:::(cleanedRules).asInstanceOf[Seq[RuleOrDeclarationAST]]
        (cleanedTuples.exists(tuple => tuple._1), RuleAST(rule.selector, newDeclarations))
      } else {
        (false, rule)
      }
    }
  }

  def limitFloats(css : Css, n : Integer) : Boolean = {
    css.rules.count(rule => rule.declarations
      .exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "float")) +
      css.rules.map(rule => countFloats(rule)).sum > n
  }

  def countFloats(rule: BaseRuleAST) : Int = {
    // Count rules with float property and sum with the sum of the float count of the nested rules
    val rules = rule.declarations.filter(rule => rule.isInstanceOf[BaseRuleAST])
    rules.count(rule => rule.asInstanceOf[BaseRuleAST].declarations
      .exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "float")) +
      rules.map(rule => countFloats(rule.asInstanceOf[BaseRuleAST])).sum
  }
}

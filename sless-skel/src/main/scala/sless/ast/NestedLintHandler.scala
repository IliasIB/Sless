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
      val newRule = rule match {
        case RuleAST(selector, _) => RuleAST(selector, declarations ++ rules)
        case RuleCommentAST(selector, _, comment) => RuleCommentAST(selector, declarations ++ rules, comment)
      }
      (newTuples.exists(tuple => tuple._1), newRule)
    }
    else {
      (false, rule)
    }
  }

  def aggregateMargins(css : Css) : (Boolean, Css) = {
    val marginRules = css.rules.partition(rule =>
      rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-left") &&
        rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-right") &&
        rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-top") &&
        rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-bottom")
    )

    val cleanedRules = marginRules._1.map(rule =>
      RuleAST(rule.selector, rule.declarations.filter(declaration =>
        !(declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-left" ||
          declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-right" ||
          declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-top" ||
          declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-bottom")
      ).toList.::(DeclarationAST(PropertyAST("margin"), ValueAST(
        rule.declarations.find(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-top").get.asInstanceOf[BaseDeclarationAST].value.value + " " +
          rule.declarations.find(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-right").get.asInstanceOf[BaseDeclarationAST].value.value + " " +
          rule.declarations.find(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-bottom").get.asInstanceOf[BaseDeclarationAST].value.value + " " +
          rule.declarations.find(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-left").get.asInstanceOf[BaseDeclarationAST].value.value
      )))))
    val newRules = marginRules._2 ++ cleanedRules

    if (marginRules._1.nonEmpty){
      (true, (new CompilableHandler).css(newRules: _*))
    } else {
      (false, css)
    }
  }

  def limitFloats(css : Css, n : Integer) : Boolean = {
    css.rules.count(rule => rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "float")) > n
  }
}

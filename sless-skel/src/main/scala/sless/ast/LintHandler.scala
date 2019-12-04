package sless.ast

import sless.dsl.LintDSL

trait LintHandler extends LintDSL {
  type Css = CssAST

  def removeEmptyRules(css : Css) : (Boolean, Css) = {
    val newRules = css.rules.filter(rule => rule.declarations.nonEmpty)
    if (newRules.size < css.rules.size){
      (true, (new CompilableHandler).css(newRules: _*))
    } else {
      (false, css)
    }
  }

  def aggregateMargins(css : Css) : (Boolean, Css) = {
    val marginRules = css.rules.groupBy(rule =>
      rule.declarations.exists(declaration => declaration.property.property == "margin-left") &&
      rule.declarations.exists(declaration => declaration.property.property == "margin-right") &&
      rule.declarations.exists(declaration => declaration.property.property == "margin-top") &&
      rule.declarations.exists(declaration => declaration.property.property == "margin-bottom")
    )

    val cleanedRules = marginRules(true).map(rule =>
      RuleAST(rule.selector, rule.declarations.filter(declaration =>
        !(declaration.property.property == "margin-left" ||
        declaration.property.property == "margin-right" ||
        declaration.property.property == "margin-top" ||
        declaration.property.property == "margin-bottom")
      ).toList.::(DeclarationAST(PropertyAST("margin"), ValueAST(
        rule.declarations.find(declaration => declaration.value.value == "margin-left") + " " +
        rule.declarations.find(declaration => declaration.value.value == "margin-right") + " " +
        rule.declarations.find(declaration => declaration.value.value == "margin-top") + " " +
        rule.declarations.find(declaration => declaration.value.value == "margin-bottom")
      )))))

    val newRules = marginRules(false) ++ cleanedRules

    if (marginRules.nonEmpty){
      (true, (new CompilableHandler).css(newRules: _*))
    } else {
      (false, css)
    }
  }

  def limitFloats(css : Css, n : Integer) : Boolean = {
    css.rules.count(rule => rule.declarations.exists(declaration => declaration.property.property == "float")) > n
  }
}

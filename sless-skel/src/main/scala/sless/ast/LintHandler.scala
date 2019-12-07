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
    val marginRules = css.rules.partition(rule =>
      rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-left") &&
      rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-right") &&
      rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-top") &&
      rule.declarations.exists(declaration => declaration.asInstanceOf[BaseDeclarationAST].property.property == "margin-bottom")
    )

    val cleanedRules = marginRules._1.map(rule =>
      RuleAST(rule.selector, rule.declarations.filter(declaration =>
        declaration.asInstanceOf[BaseDeclarationAST].property.property match {
          case "margin-left" | "margin-right" |"margin-top" |"margin-bottom" => false
          case _ => true
        }
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

package sless.ast

import sless.dsl.Compilable

object DSL extends ValueTrait with PropertyTrait with Compilable with SelectorTrait with CommentTrait {
  type Rule = RuleClass
  type Css = CssClass
  type Selector = SelectorClass
  type Declaration = DeclarationClass
  type Property = PropertyClass
  type Value = ValueClass

  def fromRules(rules: Seq[Rule]): Css = {
    new CssClass(rules)
  }
  def compile(sheet: Css): String = {
    sheet.sRules.map((rule) => (
        compile(rule)
      )).mkString("")
  }
  def compile(selector: Selector): String = {
    selector match {
      case Tipe(string) => string
      case Id(s, string) => compile(s) + "#" + string
      case Group(selectors) => selectors.map((selector) => (compile(selector))).mkString(",")
      case Child(s, selector) => compile(s) + ">" + compile(selector)
      case General(s, selector) => compile(s) + "~" + compile(selector)
      case Adjacent(s, selector) => compile(s) + "+" + compile(selector)
      case PseudoClass(s, string) => compile(s) + ":" + string
      case Descendant(s, selector) => compile(s) + " " + compile(selector)
      case PseudoElement(s, string) => compile(s) + "::" + string
      case Attribute(s, attr, string) => compile(s) + "[" + attr + "=\"" + string + "\"]"
      case ClassName(s, string) => compile(s) + "." + string
      case All => "*"
    }
  }
  def compile(declaration: Declaration): String = {
    val declarationString = declaration.sProperty.sProperty + ":" + declaration.sValue.sValue + ";"
    if (declaration.sComment.sComment != "") {
      declarationString + "/* " + declaration.sComment.sComment + " */"
    } else {
      declarationString
    }
  }
  def compile(rule: Rule): String = {
    val ruleString = compile(rule.sSelector) + "{" +
      rule.sDeclarations.map((declaration) =>
        (compile(declaration))).mkString("") + "}"
    if (rule.sComment.sComment != "") {
      "/* " + rule.sComment.sComment + " */" + ruleString
    } else {
      ruleString
    }
  }
  def pretty(sheet: Css, spaces: Int): String = {
    sheet.sRules.map((rule) => (
        pretty(rule, spaces)
      )).mkString("\n\n")
  }
  def pretty(selector: Selector): String = {
    selector match {
      case Tipe(string) => string
      case Id(s, string) => pretty(s) + "#" + string
      case Group(selectors) => selectors.map((selector) => (pretty(selector))).mkString(", ")
      case Child(s, selector) => pretty(s) + " > " + pretty(selector)
      case General(s, selector) => pretty(s) + " ~ " + pretty(selector)
      case Adjacent(s, selector) => pretty(s) + " + " + pretty(selector)
      case PseudoClass(s, string) => pretty(s) + ":" + string
      case Descendant(s, selector) => pretty(s) + " " + pretty(selector)
      case PseudoElement(s, string) => pretty(s) + "::" + string
      case Attribute(s, attr, string) => pretty(s) + "[" + attr + "=\"" + string + "\"]"
      case ClassName(s, string) => pretty(s) + "." + string
      case All => "*"
    }
  }
  def pretty(declaration: Declaration, spaces: Int): String = {
    val declarationString = (" " * spaces) +
      declaration.sProperty.sProperty + ": " + declaration.sValue.sValue + ";"
    if (declaration.sComment.sComment != "") {
      declarationString + " /* " + declaration.sComment.sComment + " */"
    } else {
      declarationString
    }
  }
  def pretty(rule: Rule, spaces: Int): String = {
    val ruleString = pretty(rule.sSelector) + " {\n" +
      rule.sDeclarations.map((declaration) =>
        (
          pretty(declaration, spaces)
          )).mkString("\n") + "\n}"
    if (rule.sComment.sComment != "") {
      "/* " + rule.sComment.sComment + " */\n" + ruleString
    } else {
      ruleString
    }
  }

  // Helper Functions
  def getRules(css: CssClass): Seq[RuleClass] = {
    css.sRules
  }
}

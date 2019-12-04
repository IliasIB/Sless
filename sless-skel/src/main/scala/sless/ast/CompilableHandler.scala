package sless.ast

import sless.dsl.{BaseDSL, Compilable}

class CompilableHandler extends BaseDSL with Compilable{
  type Css = CssAST
  type Rule = BaseRuleAST
  type Selector = SelectorAST
  type Declaration = BaseDeclarationAST

  def fromRules(rules: Seq[Rule]): Css = {
    new Css(rules)
  }

  def compile(sheet: CssAST): String = {
    sheet.rules.map(rule => compile(rule)).mkString("")
  }

  def compile(rule: Rule): String = {
    ruleComment(rule) + compile(rule.selector) + "{" +
      rule.declarations.map(declaration =>
        compile(declaration.asInstanceOf[BaseDeclarationAST])).mkString("") + "}"
  }

  def compile(selector: Selector): String = {
    selector match {
      case TipeAST(string) => string
      case IdAST(s, string) => compile(s) + "#" + string
      case GroupAST(selectors) => selectors.map((selector) => (compile(selector))).mkString(",")
      case ChildAST(s, selector) => compile(s) + ">" + compile(selector)
      case GeneralAST(s, selector) => compile(s) + "+" + compile(selector)
      case AdjacentAST(s, selector) => compile(s) + "~" + compile(selector)
      case PseudoClassAST(s, string) => compile(s) + ":" + string
      case DescendantAST(s, selector) => compile(s) + " " + compile(selector)
      case PseudoElementAST(s, string) => compile(s) + "::" + string
      case AttributeAST(s, attr, string) => compile(s) + "[" + attr + "=\"" + string + "\"]"
      case ClassNameAST(s, string) => compile(s) + "." + string
      case AllAST() => "*"
    }
  }

  def compile(declaration: Declaration): String = {
    declaration.property.property + ":" + declaration.value.value + ";" + declarationComment(declaration)
  }

  def pretty(sheet: Css, spaces: Int): String = {
    sheet.rules.map(rule => pretty(rule, spaces)).mkString("\n\n")
  }

  def pretty(selector: Selector): String = {
    selector match {
      case TipeAST(string) => string
      case IdAST(s, string) => pretty(s) + "#" + string
      case GroupAST(selectors) => selectors.map((selector) => (pretty(selector))).mkString(", ")
      case ChildAST(s, selector) => pretty(s) + " > " + pretty(selector)
      case GeneralAST(s, selector) => pretty(s) + " + " + pretty(selector)
      case AdjacentAST(s, selector) => pretty(s) + " ~ " + pretty(selector)
      case PseudoClassAST(s, string) => pretty(s) + ":" + string
      case DescendantAST(s, selector) => pretty(s) + " " + pretty(selector)
      case PseudoElementAST(s, string) => pretty(s) + "::" + string
      case AttributeAST(s, attr, string) => pretty(s) + "[" + attr + "=\"" + string + "\"]"
      case ClassNameAST(s, string) => pretty(s) + "." + string
      case AllAST() => "*"
    }
  }

  def pretty(declaration: Declaration, spaces: Int): String = {
    (" " * spaces) + declaration.property.property + ": " + declaration.value.value + ";" +
      declarationComment(declaration, pretty = true)
  }

  def pretty(rule: Rule, spaces: Int): String = {
    ruleComment(rule, pretty = true) + pretty(rule.selector) + " {\n" +
      rule.declarations.map(declaration =>
        pretty(declaration.asInstanceOf[BaseDeclarationAST], spaces)).mkString("\n") + "\n}"
  }

  protected def declarationComment(declaration: Declaration, pretty: Boolean=false): String = {
    declaration match {
      case DeclarationAST(_, _) => ""
      case DeclarationCommentAST(_, _, comment) =>  (if(pretty) " " else "") + "/* " + comment.comment + " */"
    }
  }
  protected def ruleComment(rule: Rule, pretty: Boolean=false): String = {
    rule match {
      case RuleAST(_, _) => ""
      case RuleCommentAST(_, _, comment) => "/* " + comment.comment + " */" + (if(pretty) "\n" else "")
    }
  }
}
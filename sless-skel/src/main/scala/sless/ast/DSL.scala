package sless.ast

import sless.dsl.Compilable

case class ParentException(message: String = "Root element can't have Parent") extends Exception(message)

object DSL extends ValueTrait with PropertyTrait with Compilable with SelectorTrait with CommentTrait {
  type Rule = RuleClass
  type Css = CssClass
  type Selector = SelectorClass
  type Declaration = DeclarationClass
  type Property = PropertyClass
  type Value = ValueClass
  type RuleOrDeclaration = RuleOrDeclarationClass

  def fromRules(rules: Seq[Rule]): Css = {
    new CssClass(rules)
  }
  def rectifyParent(selector: Selector, accumulated: Selector): Selector = {
    selector match {
      case Tipe(string) => Tipe(string)
      case Id(s, string) => Id(rectifyParent(s, accumulated), string)
      case Group(selectors) => Group(selectors.map((selector) => (rectifyParent(selector, accumulated))))
      case Child(s, selector) => Child(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case General(s, selector) => General(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case Adjacent(s, selector) => Adjacent(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case PseudoClass(s, string) => PseudoClass(rectifyParent(s, accumulated), string)
      case Descendant(s, selector) => Descendant(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case PseudoElement(s, string) => PseudoElement(rectifyParent(s, accumulated), string)
      case Attribute(s, attr, string) => Attribute(rectifyParent(s, accumulated), attr, string)
      case ClassName(s, string) => ClassName(rectifyParent(s, accumulated), string)
      case All => All
      case Parent =>
        if (accumulated != null) {
          accumulated
        }else {
          throw ParentException()
        }
    }
  }
  def noNullDescendant(s1: Selector, s2: Selector): Selector = {
    if (s1 != null){
      Descendant(s1, s2)
    } else {
      s2
    }
  }
  def compile(sheet: Css): String = {
    sheet.sRules.map((rule) => (
        compile(rule)
      )).mkString("")
  }
  def compile(selector: Selector, accumulated: Selector): String = {
    selector match {
      case Tipe(string) => string
      case Id(s, string) => compile(s, accumulated) + "#" + string
      case Group(selectors) => selectors.map((selector) => (compile(selector, accumulated))).mkString(",")
      case Child(s, selector) => compile(s, accumulated) + ">" + compile(selector, accumulated)
      case General(s, selector) => compile(s, accumulated) + "+" + compile(selector, accumulated)
      case Adjacent(s, selector) => compile(s, accumulated) + "~" + compile(selector, accumulated)
      case PseudoClass(s, string) => compile(s, accumulated) + ":" + string
      case Descendant(s, selector) => compile(s, accumulated) + " " + compile(selector, accumulated)
      case PseudoElement(s, string) => compile(s, accumulated) + "::" + string
      case Attribute(s, attr, string) => compile(s, accumulated) + "[" + attr + "=\"" + string + "\"]"
      case ClassName(s, string) => compile(s, accumulated) + "." + string
      case All => "*"
      case Parent =>
        if (accumulated != null) {
          compile(accumulated, null)
        }else {
          throw ParentException()
        }
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

  def compile(rule: Rule, accumulated: Selector = null): String = {
    val declarationsAndRules = rule.sDeclarations.partition {
      case DeclarationClass(_, _, _) => true
      case RuleClass(_, _, _) => false
    }
    val declarations = declarationsAndRules._1
    val rules = declarationsAndRules._2
    val ruleString = {
      if ((declarations.isEmpty && rules.isEmpty) || declarations.nonEmpty) {
        compile(rule.sSelector, accumulated) + "{" +
          declarations.map((declaration) =>
            (compile(declaration.asInstanceOf[Declaration]))).mkString("") + "}"
      } else {
        ""
      }
    }

    if (rule.sComment.sComment != "") {
      if (rules.isEmpty){
        "/* " + rule.sComment.sComment + " */" + ruleString
      } else {
        val rolledOutString = rules.map(rule2 =>
          compile(rule2.asInstanceOf[RuleClass], noNullDescendant(accumulated,
            rectifyParent(rule.asInstanceOf[RuleClass].sSelector, accumulated)))).mkString("")
        "/* " + rule.sComment.sComment + " */" + ruleString + rolledOutString
      }
    } else {
      if (rules.isEmpty){
        ruleString
      } else {
        val rolledOutString = rules.map(rule2 =>
          compile(rule2.asInstanceOf[RuleClass], noNullDescendant(accumulated,
            rectifyParent(rule.asInstanceOf[RuleClass].sSelector, accumulated)))).mkString("")
        ruleString + rolledOutString
      }
    }
  }
  def pretty(sheet: Css, spaces: Int): String = {
    sheet.sRules.map((rule) => (
        pretty(rule, spaces)
      )).mkString("\n\n")
  }
  def pretty(selector: Selector, accumulated: Selector): String = {
    selector match {
      case Tipe(string) => string
      case Id(s, string) => pretty(s, accumulated) + "#" + string
      case Group(selectors) => selectors.map((selector) => (pretty(selector, accumulated))).mkString(", ")
      case Child(s, selector) => pretty(s, accumulated) + " > " + pretty(selector, accumulated)
      case General(s, selector) => pretty(s, accumulated) + " + " + pretty(selector, accumulated)
      case Adjacent(s, selector) => pretty(s, accumulated) + " ~ " + pretty(selector, accumulated)
      case PseudoClass(s, string) => pretty(s, accumulated) + ":" + string
      case Descendant(s, selector) => pretty(s, accumulated) + " " + pretty(selector, accumulated)
      case PseudoElement(s, string) => pretty(s, accumulated) + "::" + string
      case Attribute(s, attr, string) => pretty(s, accumulated) + "[" + attr + "=\"" + string + "\"]"
      case ClassName(s, string) => pretty(s, accumulated) + "." + string
      case All => "*"
      case Parent =>
        if (accumulated != null) {
          pretty(accumulated, null)
        }else {
          throw ParentException()
        }
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
  def pretty(rule: Rule, spaces: Int, accumulated: Selector = null): String = {
    val declarationsAndRules = rule.sDeclarations.partition {
      case DeclarationClass(_, _, _) => true
      case RuleClass(_, _, _) => false
    }
    val declarations = declarationsAndRules._1
    val rules = declarationsAndRules._2
    val ruleString = {
      if ((declarations.isEmpty && rules.isEmpty) || declarations.nonEmpty) {
        pretty(rule.sSelector, accumulated) + " {\n" +
          declarations.map((declaration) =>
            (pretty(declaration.asInstanceOf[Declaration], spaces))).mkString("\n") + "\n}"
      } else {
        ""
      }
    }

    if (rule.sComment.sComment != "") {
      if (rules.isEmpty){
        if (ruleString != "")
          "/* " + rule.sComment.sComment + " */\n" + ruleString
        else
          ""
      } else {
        val rolledOutString = rules.map(rule2 =>
          pretty(rule2.asInstanceOf[RuleClass], spaces, noNullDescendant(accumulated,
            rectifyParent(rule.asInstanceOf[RuleClass].sSelector, accumulated)))).mkString("\n\n")
        if (ruleString != "")
          "/* " + rule.sComment.sComment + " */\n" + ruleString + "\n\n" + rolledOutString
        else
          "/* " + rule.sComment.sComment + " */\n" + rolledOutString
      }
    } else {
      if (rules.isEmpty){
        ruleString
      } else {
        val rolledOutString = rules.map(rule2 =>
          pretty(rule2.asInstanceOf[RuleClass], spaces, noNullDescendant(accumulated,
            rectifyParent(rule.asInstanceOf[RuleClass].sSelector, accumulated)))).mkString("\n\n")
        if (ruleString != "")
          ruleString + "\n\n" + rolledOutString
        else
          rolledOutString
      }
    }
  }

  // Helper Functions
  def getRules(css: CssClass): Seq[RuleClass] = {
    css.sRules
  }
}

package sless.ast

class NestedCompilableHandler extends CompilableHandler {

  override def compile(rule: Rule): String = {
    if (rule.declarations.exists(declaration => declaration.isInstanceOf[BaseRuleAST])) {
      compile(rule, null)
    } else {
      super.compile(rule)
    }
  }

  def compile(rule: Rule, accumulated: Selector): String = {
    val declarationsAndRules = rule.declarations.partition {
      case _: BaseDeclarationAST => true
      case _: BaseRuleAST => false
    }
    val declarations = declarationsAndRules._1.asInstanceOf[Seq[BaseDeclarationAST]]
    val rules = declarationsAndRules._2.asInstanceOf[Seq[BaseRuleAST]]
    val ruleString = {
      if ((declarations.isEmpty && rules.isEmpty) || declarations.nonEmpty) {
        (if (!containsParent(rule.selector))
          compile(noNullDescendant(accumulated, rule.selector))
        else
          compile(rule.selector, accumulated)) + "{" +
          declarations.map(declaration =>
            compile(declaration)).mkString("") + "}"
      } else {
        ""
      }
    }

    if (rules.isEmpty){
      ruleComment(rule) + ruleString
    } else {
      ruleComment(rule) + ruleString + rules.map(rule2 =>
         compile(rule2,
           if (!containsParent(rule.selector))
             noNullDescendant(accumulated, rule.selector)
           else
             rectifyParent(rule.selector, accumulated))).mkString("")
    }
  }

  def compile(selector: Selector, accumulated: Selector): String = {
    selector match {
      case TipeAST(string) => string
      case IdAST(s, string) => compile(s, accumulated) + "#" + string
      case GroupAST(selectors) => selectors.map(selector => compile(selector, accumulated)).mkString(",")
      case ChildAST(s, selector) => compile(s, accumulated) + ">" + compile(selector, accumulated)
      case GeneralAST(s, selector) => compile(s, accumulated) + "~" + compile(selector, accumulated)
      case AdjacentAST(s, selector) => compile(s, accumulated) + "+" + compile(selector, accumulated)
      case PseudoClassAST(s, string) => compile(s, accumulated) + ":" + string
      case DescendantAST(s, selector) => compile(s, accumulated) + " " + compile(selector, accumulated)
      case PseudoElementAST(s, string) => compile(s, accumulated) + "::" + string
      case AttributeAST(s, attr, string) => compile(s, accumulated) + "[" + attr + "=\"" + string + "\"]"
      case ClassNameAST(s, string) => compile(s, accumulated) + "." + string
      case AllAST() => "*"
      case ParentAST() =>
        if (accumulated != null) {
          compile(accumulated, null)
        }else {
          throw ParentException()
        }
    }
  }

  private def rectifyParent(selector: Selector, accumulated: Selector): Selector = {
    selector match {
      case TipeAST(string) => TipeAST(string)
      case IdAST(s, string) => IdAST(rectifyParent(s, accumulated), string)
      case GroupAST(selectors) => GroupAST(selectors.map(selector => rectifyParent(selector, accumulated)))
      case ChildAST(s, selector) => ChildAST(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case GeneralAST(s, selector) => GeneralAST(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case AdjacentAST(s, selector) => AdjacentAST(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case PseudoClassAST(s, string) => PseudoClassAST(rectifyParent(s, accumulated), string)
      case DescendantAST(s, selector) => DescendantAST(rectifyParent(s, accumulated), rectifyParent(selector, accumulated))
      case PseudoElementAST(s, string) => PseudoElementAST(rectifyParent(s, accumulated), string)
      case AttributeAST(s, attr, string) => AttributeAST(rectifyParent(s, accumulated), attr, string)
      case ClassNameAST(s, string) => ClassNameAST(rectifyParent(s, accumulated), string)
      case AllAST() => AllAST()
      case ParentAST() =>
        if (accumulated != null) {
          accumulated
        }else {
          throw ParentException()
        }
    }
  }

  private  def noNullDescendant(s1: Selector, s2: Selector): Selector = {
    if (s1 != null){
      DescendantAST(s1, s2)
    } else {
      s2
    }
  }

  private def containsParent(selector: Selector): Boolean = {
    selector match {
      case TipeAST(_) => false
      case IdAST(s, _) => containsParent(s)
      case GroupAST(selectors) => selectors.exists(selector => containsParent(selector))
      case ChildAST(s, selector) => containsParent(s) || containsParent(selector)
      case GeneralAST(s, selector) => containsParent(s) || containsParent(selector)
      case AdjacentAST(s, selector) => containsParent(s) || containsParent(selector)
      case PseudoClassAST(s, _) => containsParent(s)
      case DescendantAST(s, selector) => containsParent(s) || containsParent(selector)
      case PseudoElementAST(s, _) => containsParent(s)
      case AttributeAST(s, _, _) => containsParent(s)
      case ClassNameAST(s, _) => containsParent(s)
      case AllAST() => false
      case ParentAST() => true
    }
  }

  override def pretty(sheet: Css, spaces: Int): String = {
    sheet.rules.map(rule => pretty(rule, spaces, null)).mkString("\n\n")
  }

  def pretty(selector: Selector, accumulated: Selector): String = {
    selector match {
      case TipeAST(string) => string
      case IdAST(s, string) => pretty(s, accumulated) + "#" + string
      case GroupAST(selectors) => selectors.map(selector => pretty(selector, accumulated)).mkString(", ")
      case ChildAST(s, selector) => pretty(s, accumulated) + " > " + pretty(selector, accumulated)
      case GeneralAST(s, selector) => pretty(s, accumulated) + " ~ " + pretty(selector, accumulated)
      case AdjacentAST(s, selector) => pretty(s, accumulated) + " + " + pretty(selector, accumulated)
      case PseudoClassAST(s, string) => pretty(s, accumulated) + ":" + string
      case DescendantAST(s, selector) => pretty(s, accumulated) + " " + pretty(selector, accumulated)
      case PseudoElementAST(s, string) => pretty(s, accumulated) + "::" + string
      case AttributeAST(s, attr, string) => pretty(s, accumulated) + "[" + attr + "=\"" + string + "\"]"
      case ClassNameAST(s, string) => pretty(s, accumulated) + "." + string
      case AllAST() => "*"
      case ParentAST() =>
        if (accumulated != null) {
          pretty(accumulated, null)
        }else {
          throw ParentException()
        }
    }
  }

  def pretty(rule: Rule, spaces: Int, accumulated: Selector = null): String = {
    val declarationsAndRules = rule.declarations.partition {
      case _: BaseDeclarationAST => true
      case _: BaseRuleAST => false
    }
    val declarations = declarationsAndRules._1.asInstanceOf[Seq[BaseDeclarationAST]]
    val rules = declarationsAndRules._2.asInstanceOf[Seq[BaseRuleAST]]
    val ruleString = {
      if ((declarations.isEmpty && rules.isEmpty) || declarations.nonEmpty) {
        (if (!containsParent(rule.selector))
          pretty(noNullDescendant(accumulated, rule.selector))
        else
          pretty(rule.selector, accumulated)) + " {\n" +
          declarations.map(declaration =>
            pretty(declaration, spaces)).mkString("\n") + "\n}"
      } else {
        ""
      }
    }

    if (rules.isEmpty){
      ruleComment(rule, pretty = true) + ruleString
    } else {
      ruleComment(rule, pretty = true) + ruleString + rules.map(rule2 =>
        pretty(rule2, spaces,
          if (!containsParent(rule.selector))
            noNullDescendant(accumulated, rule.selector)
          else
            rectifyParent(rule.selector, accumulated))).mkString("\n\n")
    }
  }
}

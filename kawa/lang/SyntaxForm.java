package kawa.lang;
import gnu.expr.ScopeExp;

public class SyntaxForm
{
  Object form;
  Translator tr;
  ScopeExp scope;

  public final Translator getTranslator() { return tr; }
}

package kawa.lang;
import gnu.expr.ScopeExp;

/** A "syntatic closure" - a syntax form with its compilation environment. */

public class SyntaxForm
{
  Object form;
  Translator tr;
  ScopeExp scope;  /* Not currently used. */

  public final Translator getTranslator() { return tr; }
}

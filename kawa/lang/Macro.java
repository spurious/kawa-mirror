package kawa.lang;

public abstract class Macro extends Syntax
{
  public abstract Object expand (Pair form, Translator tr);

  public gnu.expr.Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.rewrite(expand(form, tr));
  }
}

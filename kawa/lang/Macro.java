package kawa.lang;

public abstract class Macro extends Syntax
{
  public abstract Object expand (Object obj, Translator tr);

  public gnu.expr.Expression rewrite (Object obj, Translator tr)
  {
    return tr.rewrite(expand(obj, tr));
  }
}

package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.expr.Expression;
import gnu.expr.TryExp;
import gnu.kawa.util.Pair;

public class UnwindProtect extends Syntax
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError("invalid syntax for unwind-protect");
    Pair pair = (Pair) obj;
    return new TryExp(tr.rewrite(pair.car), tr.rewrite_body(pair.cdr));
  }
}

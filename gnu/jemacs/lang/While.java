package gnu.jemacs.lang;
import gnu.expr.*;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;

/** Implement the ELisp `while' syntax form. */

public class While extends Syntax implements Printable
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    if (! (obj instanceof Pair))
      return tr.syntaxError("missing arguments for while");
    tr.mustCompileHere();
    form = (Pair) obj;
    return Expression.makeWhile(tr.rewrite(form.car),
				tr.rewrite_body(form.cdr));
  }
}

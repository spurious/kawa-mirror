package kawa.standard;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

public class thisRef extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (form.cdr == LList.Empty)
      {
	ScopeExp sc = tr.currentScope();
	while (sc != null && ! (sc instanceof ClassExp))
	  sc = sc.outer;
	if (sc == null)
	  tr.error('w', "use fo this not inside a class");
	return new ThisExp(sc);
      }
    else
      return tr.syntaxError("this with paramater not implemented");
  }
}

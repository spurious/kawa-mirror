package kawa.standard;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

public class thisRef extends Syntax
{
  public static final thisRef thisSyntax = new thisRef();
  static { thisSyntax.setName("this"); }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (form.cdr == LList.Empty)
      {
	ScopeExp sc = tr.currentScope();
        for (;;)
          {
            if (sc == null)
              {
                tr.error('w', "use of 'this' not inside a class");
                break;
              }
            if (sc instanceof LambdaExp && ((LambdaExp) sc).isClassMethod())
              {
                Declaration firstParam = sc.firstDecl();
                if (firstParam != null && firstParam.isThisParameter())
                  return new ThisExp(firstParam);
                tr.error('w', "use of 'this' inside static method");
                break;
              }
            sc = sc.outer;
          }
	return new ThisExp();
      }
    else
      return tr.syntaxError("this with paramater not implemented");
  }
}

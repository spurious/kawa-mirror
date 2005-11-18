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
        LambdaExp method = tr.curMethodLambda;
        if (method == null)
          tr.error('w', "use of 'this' not inside a class");
        else
          {
            Declaration firstParam = method.firstDecl();
            if (firstParam != null && firstParam.isThisParameter())
              return new ThisExp(firstParam);
            tr.error('w', "use of 'this' inside static method");
          }
	return new ThisExp();
      }
    else
      return tr.syntaxError("this with paramater not implemented");
  }
}

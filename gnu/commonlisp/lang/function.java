package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

public class function extends Syntax
{
  Syntax lambda;

  public function(Syntax lambda)
  {
    this.lambda = lambda;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    if (obj instanceof Pair)
      {
	Pair pair = (Pair) obj;
	if (pair.cdr != LList.Empty)
	  return tr.syntaxError("too many forms after 'function'");
	Object name = pair.car;
	if (name instanceof String || name instanceof Symbol)
	  {
	    ReferenceExp rexp = new ReferenceExp(name);
	    rexp.setProcedureName(true);
	    return rexp;
	  }
	if (name instanceof Pair)
	  {
	    pair = (Pair) name;
	    name = pair.car;
	    if (name instanceof String ? "lambda".equals(name)
		: (name instanceof Symbol
		   && "lambda".equals(((Symbol) name).getName())))
	      return lambda.rewriteForm(pair, tr);
	  }
      }
    return tr.syntaxError("function must be followed by name or lambda expression");
  }
}

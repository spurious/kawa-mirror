package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class lambda extends Syntax implements Printable
{
  static private Pattern pattern = new VarListPat (1);

  public static final String optionalKeyword = "&optional";
  public static final String restKeyword = "&rest";

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("missing formals in lambda");
    int old_errors = tr.getMessages().getErrorCount();
    LambdaExp lexp = new LambdaExp();
    rewrite(lexp, match[0], match[1], tr);
    if (tr.getMessages().getErrorCount() > old_errors)
      return new ErrorExp("bad lambda expression");
    return lexp;
  }

  /**
   * Higher-level constructor, that does the re-writing.
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param tr the (Scheme) Translator
   */
  // FIXME make method of Translator
  public static void rewrite(LambdaExp lexp, Object formals, Object body, Translator tr)
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    int opt_args = -1;
    int rest_args = -1;
    Pair pair;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
        // An initial pass to count the parameters.
	if (pair.car == optionalKeyword)
	  {
	    if (opt_args >= 0)
	      {
		tr.syntaxError ("multiple &optional in parameter list");
		return;
	      }
	    else if (rest_args >= 0)
	      {
		tr.syntaxError ("&optional after &rest");
		return;
	      }
	    opt_args = 0;
	  }
	else if (pair.car == restKeyword)
	  {
	    if (rest_args >= 0)
	      {
		tr.syntaxError ("multiple &rest in parameter list");
		return;
	      }
	    rest_args = 0;
	  }
        else if (pair.car == "::" // && "::" is unbound FIXME
                 && pair.cdr instanceof Pair)
          pair = (Pair) pair.cdr;
	else if (rest_args >= 0)
	  rest_args++;
	else if (opt_args >= 0)
	  opt_args++;
	else
	  lexp.min_args++;
	bindings = pair.cdr;
      }
    if (bindings instanceof String)
      {
	if (opt_args >= 0 || rest_args >= 0)
	  {
	    tr.syntaxError ("dotted rest-arg after &optional or &rest");
	    return;
	  }
	rest_args = 1;
      }
    else if (bindings != LList.Empty)
      {
	tr.syntaxError ("misformed formals in lambda");
	return;
      }
    if (rest_args > 1)
      {
	tr.syntaxError ("multiple #!rest parameters");
        return;
      }
    if (opt_args < 0)
      opt_args = 0;
    if (rest_args < 0)
      rest_args = 0;
    if (rest_args > 0)
      lexp.max_args = -1;
    else   // Is this useful?
      lexp.max_args = lexp.min_args + opt_args;
    if (opt_args > 0)
      lexp.defaultArgs = new Expression[opt_args];

    tr.push(lexp);
    bindings = formals;
    int i = 0;
    opt_args = 0;
    Object mode = null;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == optionalKeyword || pair.car == restKeyword)
	  {
	    mode = pair.car;
	    continue;
	  }
        Pair p;
	if (! (pair.car instanceof String))
	  {
	    tr.syntaxError ("parameter is not a symbol");
	    return;
	  }
	String name = (String) pair.car;
	if (mode == optionalKeyword)
	  lexp.defaultArgs[opt_args++] = ELisp.nilExpr;
	Declaration decl = lexp.addDeclaration (name);
        if (bindings instanceof PairWithPosition)
          {
            PairWithPosition declPos = (PairWithPosition) bindings;
            decl.setFile(declPos.getFile());
            decl.setLine(declPos.getLine(), declPos.getColumn());
          }
	if (mode == restKeyword)
	  decl.setType(Compilation.scmListType);
	decl.noteValue(null);  // Does not have a known value.
	tr.push(decl);
      }
    if (bindings instanceof String)
      {
	Declaration decl = lexp.addDeclaration ((String) bindings);
	decl.setType(Compilation.scmListType);
	decl.noteValue (null);  // Does not have a known value.
	tr.push(decl);
      }
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());
    lexp.body = tr.rewrite_body (body);
    tr.pop(lexp);
  }


  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}

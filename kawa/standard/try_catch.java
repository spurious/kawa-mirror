package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Utility method for try-catch.
 */

public class try_catch
{
  public static Expression rewrite (Object try_part, Object clauses)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    Expression try_part_exp = tr.rewrite(try_part);
    CatchClause prev = null;
    CatchClause chain = null;
    FVector vec = (FVector) clauses;
    int n = vec.size();
    for (int i = 0;  i < n;  i++)
      {
	Expression cl = tr.rewrite(vec.get(i));
	if (cl instanceof ErrorExp)
	  return cl;
	if (! (cl instanceof LetExp))
	  return tr.syntaxError("internal error with try-catch");
	CatchClause ccl = new CatchClause((LetExp) cl);
	if (prev == null)
	  chain = ccl;
	else
	  prev.setNext(ccl);
	prev = ccl;
      }
    if (try_part_exp instanceof ErrorExp)
      return try_part_exp; 
    TryExp texp = new TryExp(try_part_exp, null);
    texp.setCatchClauses(chain);
    return texp;
  }
}

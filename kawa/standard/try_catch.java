package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes "try-catch".
 * (try-catch try-clause (var type exp*)+)
 * @author	Per Bothner
 */

public class try_catch extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError("missing try-clause in try-catch");
    Pair pair = (Pair) obj;
    TryExp try_exp = new TryExp (tr.rewrite(pair.car), null);

    CatchClause last = null;
    for (;;)
      {
	obj = pair.cdr;
	if (obj == LList.Empty)
	  break;
	if (! (obj instanceof Pair))
	  return tr.syntaxError("improper list in try-catch");
	pair = (Pair) obj;
	Object try_obj = pair.car;
	if (try_obj instanceof Pair)
	  {
	    Pair try_pair = (Pair) try_obj;
	    if (! (try_pair.car instanceof String))
	      return tr.syntaxError("invalid identifier of catch clause");
	    String name = (String) try_pair.car;
	    try_obj = try_pair.cdr;
	    if (try_obj instanceof Pair)
	      {
		try_pair = (Pair) try_obj;
		Type type = prim_method.exp2Type (try_pair.car, tr);
		if (! (type instanceof ClassType))
		  return tr.syntaxError("catch clause type not a class type");
		CatchClause clause = new CatchClause (name, (ClassType) type);
		tr.push(clause);
		clause.setBody(tr.rewrite_body(try_pair.cdr));
		tr.pop(clause);
		if (last == null)
		  try_exp.setCatchClauses(clause);
		else
		  last.setNext(clause);
		last = clause;
		continue;
	      }
	  }
	return tr.syntaxError("invalid syntax for catch clause in try-catch");
      }
    return try_exp;
  }
}

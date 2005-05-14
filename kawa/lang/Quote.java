package kawa.lang;
import java.util.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.Invoke;
import gnu.bytecode.ClassType;
import gnu.kawa.lispexpr.LispLanguage;

/**
 * The Syntax transformer that re-writes the "quote" "quasiquote" primitive.
 * In both cases recursively resolves SyntaxForm wrappers and resolves
 * namespaces of symbols.  In the case of quasiquote also handles unquoting.
 * @author	Per Bothner
 */

public class Quote extends Syntax implements Printable
{
  public static final Quote plainQuote = new Quote("quote", false);
  public static final Quote quasiQuote = new Quote("quasiquote", true);

  public Quote (String name, boolean isQuasi)
  {
    super(name);
    this.isQuasi = isQuasi;
  }

  /** An initial value for 'depth' for plain (non-quasi) quote,
   * with namespace resolution. */
  static final int QUOTE_DEPTH = -1;

  /** An initial value for 'depth' for plain (non-quasi) quote,
   * without namespace resolution. */
  static final int DATUM_DEPTH = -2;

  /** True for quasiquote; false for plain quote. */
  boolean isQuasi;

  static Object expand (Object template, int depth, Translator tr)
  {
    /* #ifndef JAVA2 */
    // Object seen = null;
    /* #endif */
    /* #ifdef JAVA2 */
    IdentityHashMap seen = new IdentityHashMap();
    /* #endif */
    return expand(template, depth, null, seen, tr);
  }

  /** Quote an object (without namespace-expansion).
   * Basically just recursively removes SyntaxForm wrappers. */
  public static Object quote (Object obj, Translator tr)
  {
    return expand(obj, DATUM_DEPTH, tr);
  }

  /** Quote an object (without namespace-expansion).
   * Basically just recursively removes SyntaxForm wrappers. */
  public static Object quote (Object obj)
  {
    return expand(obj, DATUM_DEPTH, (Translator) Compilation.getCurrent());
  }

  static Expression coerceExpression (Object val)
  {
    return val instanceof Expression ? (Expression) val : new QuoteExp (val);
  }

  static Object expand_pair (Pair list, int depth, SyntaxForm syntax,
			     Object seen, Translator tr)
  {
    Pair pair = list;
    Object cdr;
    Object rest;
    for (;;)
      {
        // This would be simpler as palin recusion, but we try to iterate
        // over the given list, partly for speed, but more importantly
        // to avoid stack overflow in the case of long lists.
        rest = pair;
        // We're currently examining pair, which is the n'th cdr of list.
        // All previous elements (cars) are returned identically by expand.
        // What makes things complicated is that to the extent that no changes
        // are needed, we want to return the input list as-is.
        if (depth < 0)
          {
          }
        else if (tr.matches(pair.car, LispLanguage.quasiquote_sym))
          depth++;
        else if (tr.matches(pair.car, LispLanguage.unquote_sym))
          {
            depth--;
            Pair pair_cdr;
            if (! (pair.cdr instanceof Pair)
                || (pair_cdr = (Pair) pair.cdr).cdr != LList.Empty)
              return tr.syntaxError ("invalid used of " + pair.car +
				     " in quasiquote template");
            if (depth == 0)
              {
                cdr = tr.rewrite_car(pair_cdr, syntax);
                break;
              }
          }
        else if (tr.matches(pair.car, LispLanguage.unquotesplicing_sym))
          return tr.syntaxError ("invalid used of " + pair.car +
				 " in quasiquote template");
        if (pair.car instanceof Pair && depth > QUOTE_DEPTH)
          {
            Pair pair_car = (Pair)pair.car;
            if (tr.matches(pair_car.car, LispLanguage.unquotesplicing_sym)
                && depth == 1)
              {
                Pair pair_car_cdr;
                if (! (pair_car.cdr instanceof Pair)
                    || (pair_car_cdr = (Pair) pair_car.cdr).cdr != LList.Empty)
                  return tr.syntaxError ("invalid used of " + pair_car.car +
                                         " in quasiquote template");
                Expression[] args = new Expression[2];
                args[0] = tr.rewrite_car(pair_car_cdr, syntax);
                Object expanded_cdr = expand (pair.cdr, depth, syntax, seen, tr);
                args[1] = coerceExpression (expanded_cdr);
                rest = pair;
                cdr = Invoke.makeInvokeStatic(appendType, "append", args);
                break;
              }
          }
        Object car = expand (pair.car, depth, syntax, seen, tr);
        if (car == pair.car)
          {
            rest = pair.cdr;
            if (rest instanceof Pair)
              {
                pair = (Pair) rest;
                continue;
              }
            cdr = expand(rest, depth, syntax, seen, tr);
            break;
          }
        cdr = expand (pair.cdr, depth, syntax, seen, tr);
        if (car instanceof Expression || cdr instanceof Expression)
          {
            Expression[] args = new Expression[2];
            args[0] = coerceExpression(car);
            args[1] = coerceExpression(cdr);
            cdr = Invoke.makeInvokeStatic(Compilation.typePair, "make", args);
          }
        else
          cdr = Translator.makePair(pair, car, cdr);
        break;
      }
    // rest is the n'th cdr of list.  cdr is the expansion of rest.
    // The first n cars of list are returned identically by expand.
    // These do need to be copied because cdr!=rest.
    if (list == rest)
      return cdr;
    Pair p = list;
    Pair prev = null;
    for (;;)
      {
        Pair q = Translator.makePair(p, p.car, null);
        if (prev == null)
          list = q;
        else
          prev.cdr = q;
        prev = q;
        if (p.cdr == rest)
          break;
        p = (Pair) p.cdr;
      }
    if (cdr instanceof Expression)
      {
        Expression[] args = new Expression[2];
        args[1] = (Expression) cdr;
        if (prev == list)
          {
            // The n==1 case: Only a single pair before rest.
            args[0] = new QuoteExp(list.car);
	    return Invoke.makeInvokeStatic(Compilation.typePair, "make", args);
          }
        else
          {
            prev.cdr = LList.Empty;
            args[0] = new QuoteExp(list);
	    return Invoke.makeInvokeStatic(appendType, "append", args);
          }
      }
    else
      {
        prev.cdr = cdr;
      }
    return list;
  }

  private static final Object WORKING = new String("(working)");
  private static final Object CYCLE = new String("(cycle)");

  /** Backquote-expand a template.
   * @param template the quasiquoted template to expand
   * @param depth the (net) number of quasiquotes we are inside.
   *   The values QUOTE_DEPTH and DATUM_DEPTH are special cases
   *   when we're inside a quote rather than a quasiquote.
   * @param tr the rewrite context
   * @return the expanded Expression (the result can be a non-expression,
   *   in which case it is implicitly a QuoteExp).
   */
  static Object expand (Object template, int depth,
			SyntaxForm syntax, Object seen, Translator tr)
  {
    boolean resolveNamespaces = depth > DATUM_DEPTH;
    /* #ifdef JAVA2 */
    IdentityHashMap map = (IdentityHashMap) seen;
    Object old = map.get(template);
    if (old == WORKING)
      {
	map.put(template, CYCLE);
	return old;
      }
    else if (old == CYCLE)
      {
	return old;
      }
    else if (old != null)
      return old;
    /* #endif */
    Object result;
    if (template instanceof Pair)
      result = expand_pair ((Pair) template, depth, syntax, seen, tr);
    else if (template instanceof SyntaxForm)
      {
	syntax = (SyntaxForm) template;
	result = expand(syntax.form, depth, syntax, seen, tr);
      }
    else if (template instanceof FVector)
      {
	FVector vector = (FVector) template;
	int n = vector.size();
	Object[] buffer = new Object[n];
	// For each element, the state is one of these four:
	// 0: the expanded element is the same as the original
	// 1: the expanded element is a constant
	// 2: the expanded element is neither constant nor a slice
	// 3: the element is sliced in
	byte[] state = new byte[n];
	byte max_state = 0;
	for (int i = 0;  i < n; i++)
	  {
	    Object element = vector.get(i);
	    int element_depth = depth;
	    Pair pair;
	    if (element instanceof Pair && depth > QUOTE_DEPTH
		&& tr.matches((pair = (Pair)element).car,
			      LispLanguage.unquotesplicing_sym)
		&& --element_depth == 0)
	      {
		Pair pair_cdr;
		if (! (pair.cdr instanceof Pair)
		    || (pair_cdr = (Pair) pair.cdr).cdr != LList.Empty)
		  return tr.syntaxError ("invalid used of " + pair.car +
					     " in quasiquote template");
		buffer[i] = tr.rewrite_car(pair_cdr, syntax);
		state[i] = 3;
	      }
	    else
	      {
		buffer [i] = expand (element, element_depth, syntax, seen, tr);
		if (buffer[i] == element)
		  state[i] = 0;
		else if (buffer[i] instanceof Expression)
		  state[i] = 2;
		else
		  state[i] = 1;
	      }
	    if (state[i] > max_state)
	      max_state = state[i];
	  }
	if (max_state == 0)
	  result = vector;
	else if (max_state == 1)
	  result = new FVector (buffer);
	else
	  {
	    Expression[] args = new Expression[n];
	    for (int i = 0;  i < n;  i++)
	      {
		if (state[i] == 3)
		  args[i] = (Expression) buffer[i];
		else if (max_state < 3)
		  args[i] = coerceExpression (buffer[i]);
		else if (state[i] < 2)
		  {
		    Object[] arg1 = new Object[1];
		    arg1[0] = buffer[i];
		    args[i] = new QuoteExp (new FVector (arg1));
		  }
		else
		  {
		    Expression[] arg1 = new Expression[1];
		    arg1[0] = (Expression) buffer[i];
		    args[i] = Invoke.makeInvokeStatic(vectorType, "vector", arg1);
		  }
	      }
	    if (max_state < 3)
	      result = Invoke.makeInvokeStatic(vectorType, "vector", args);
	    else
	      result = Invoke.makeInvokeStatic(vectorAppendType, "apply", args);
	  }
      }
    else if (resolveNamespaces && template instanceof String)
      result = tr.namespaceResolve((String) template);
    else
      result = template;
    /* #ifdef JAVA2 */
    if (template != result && map.get(template) == CYCLE)
      tr.error('e', "cycle in non-literal data");
    map.put(template, result);
    /* #endif */
    return result;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Pair pair;
    if (! (obj instanceof Pair)
	|| (pair = (Pair) obj).cdr != LList.Empty)
      return tr.syntaxError ("wrong number of arguments to quasiquote");
    return coerceExpression(expand(pair.car, isQuasi ? 1 : QUOTE_DEPTH, tr));
  }

  static final ClassType appendType = ClassType.make("kawa.standard.append");
  static final ClassType vectorType = ClassType.make("kawa.lib.vectors");
  static final ClassType vectorAppendType
    = ClassType.make("kawa.standard.vector_append");
}

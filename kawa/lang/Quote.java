package kawa.lang;
import java.util.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.Invoke;
import gnu.bytecode.ClassType;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.kawa.functions.GetNamedPart;

/**
 * The Syntax transformer that re-writes the "quote" "quasiquote" primitive.
 * In both cases recursively resolves SyntaxForm wrappers and resolves
 * namespaces of symbols.  In the case of quasiquote also handles unquoting.
 * @author	Per Bothner
 */

public class Quote extends Syntax
{
  public static final Quote plainQuote = new Quote("quote", false);
  public static final Quote quasiQuote = new Quote("quasiquote", true);

  public Quote (String name, boolean isQuasi)
  {
    super(name);
    this.isQuasi = isQuasi;
  }

  /** An initial value for 'depth' for plain (non-quasi) quote. */
  protected static final int QUOTE_DEPTH = -1;

  /** True for quasiquote; false for plain quote. */
  protected boolean isQuasi;

  protected Object expand (Object template, int depth, Translator tr)
  {
    /* #ifdef use:java.util.IdentityHashMap */ 
    IdentityHashMap seen = new IdentityHashMap();
    /* #else */
    // Object seen = null;
    /* #endif */
    return expand(template, depth, null, seen, tr);
  }

  /** Quote an object (without namespace-expansion).
   * Basically just recursively removes SyntaxForm wrappers. */
  public static Object quote (Object obj, Translator tr)
  {
    return plainQuote.expand(obj, QUOTE_DEPTH, tr);
  }

  /** Quote an object (without namespace-expansion).
   * Basically just recursively removes SyntaxForm wrappers. */
  public static Object quote (Object obj)
  {
    return plainQuote.expand(obj, QUOTE_DEPTH, (Translator) Compilation.getCurrent());
  }

  protected Expression coerceExpression (Object val, Translator tr)
  {
    return val instanceof Expression ? (Expression) val : leaf(val, tr);
  }

  protected Expression leaf (Object val, Translator tr)
  {
    return new QuoteExp(val);
  }

  protected boolean expandColonForms ()
  {
    return true;
  }

  Object expand_pair (Pair list, int depth, SyntaxForm syntax,
                      Object seen, Translator tr)
  {
    Pair pair = list;
    Object cdr;
    Object rest;
    for (;;)
      {
        // This would be simpler as plain recursion, but we try to iterate
        // over the given list, partly for speed, but more importantly
        // to avoid stack overflow in the case of long lists.
        rest = pair;
        Pair p1, p2;
        // We're currently examining pair, which is the n'th cdr of list.
        // All previous elements (cars) are returned identically by expand.
        // What makes things complicated is that to the extent that no changes
        // are needed, we want to return the input list as-is.
        if (expandColonForms()
            && tr.matches(pair.car, syntax, LispLanguage.lookup_sym)
            && pair.cdr instanceof Pair
            && (p1 = (Pair) pair.cdr) instanceof Pair
            && (p2 = (Pair) p1.cdr) instanceof Pair
            && p2.cdr == LList.Empty)
          {
            Expression part1 = tr.rewrite_car(p1, false);
            Expression part2 = tr.rewrite_car(p2, false);
            Symbol sym = tr.namespaceResolve(part1, part2);
            String combinedName;
            if (sym != null)
              ;
            else if (part1 instanceof ReferenceExp
                     && part2 instanceof QuoteExp)
              sym = tr.getGlobalEnvironment().getSymbol(((ReferenceExp) part1).getName() + ':' + ((QuoteExp) part2).getValue().toString());
            else if ((combinedName = GetNamedPart.combineName(part1, part2)) != null)
              sym = tr.getGlobalEnvironment().getSymbol(combinedName);
            else
              {
                Object save = tr.pushPositionOf(pair);
                tr.error('e', "'"+p1.car+"' is not a valid prefix");
                tr.popPositionOf(save);
              }
            cdr = sym;
            break;
          }
        else if (depth < 0)
          {
          }
        else if (tr.matches(pair.car, syntax, LispLanguage.quasiquote_sym))
          depth++;
        else if (tr.matches(pair.car, syntax, LispLanguage.unquote_sym))
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
        else if (tr.matches(pair.car, syntax, LispLanguage.unquotesplicing_sym))
          return tr.syntaxError ("invalid used of " + pair.car +
				 " in quasiquote template");
        if (depth == 1 && pair.car instanceof Pair)
          {
            Object form = pair.car;
            SyntaxForm subsyntax = syntax;
            while (form instanceof SyntaxForm)
              {
                subsyntax = (SyntaxForm) form;
                form = subsyntax.form;
              }
            int splicing = -1;
            if (form instanceof Pair)
              {
                Object op = ((Pair) form).car;
                if (tr.matches(op, subsyntax, LispLanguage.unquote_sym))
                  splicing = 0;
                else if (tr.matches(op, subsyntax, LispLanguage.unquotesplicing_sym))
                  splicing = 1;
              }
            if (splicing >= 0)
              {
                form = ((Pair) form).cdr; // skip "unquote[splicing]".
                Vector vec = new Vector();
                cdr = null;
                // R5RS allows only a single argument.  But
                // see Bawden: Quasiquotation in Lisp (1999), Appendix B.
                for (;;)
                  {
                    if (form instanceof SyntaxForm)
                      {
                        subsyntax = (SyntaxForm) form;
                        form = subsyntax.form;
                      }
                    if (form == LList.Empty)
                      break;
                    if (form instanceof Pair)
                      {
                        vec.addElement(tr.rewrite_car((Pair) form, subsyntax));
                        form = ((Pair) form).cdr;
                      }
                    else
                      return tr.syntaxError("improper list argument to unquote");
                  }
                int nargs = vec.size() + 1;
                cdr = expand(pair.cdr, 1, syntax, seen, tr);
                if (nargs > 1)
                  {
                    Expression[] args = new Expression[nargs];
                    vec.copyInto(args);
                    args[nargs-1] = coerceExpression(cdr, tr);
                    String method = splicing == 0 ? "consX" : "append";
                    cdr = Invoke.makeInvokeStatic(quoteType, method, args);
                  }
                rest = pair;
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
            args[0] = coerceExpression(car, tr);
            args[1] = coerceExpression(cdr, tr);
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
            args[0] = leaf(list.car, tr);
	    return Invoke.makeInvokeStatic(Compilation.typePair, "make", args);
          }
        else
          {
            prev.cdr = LList.Empty;
            args[0] = leaf(list, tr);
	    return Invoke.makeInvokeStatic(quoteType, "append", args);
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
   * @param depth - the (net) number of quasiquotes we are inside.
   *   The value QUOTE_DEPTH is a special case when we're inside
   *   a quote rather than a quasiquote.
   * @param tr the rewrite context
   * @return the expanded Expression (the result can be a non-expression,
   *   in which case it is implicitly a QuoteExp).
   */
  Object expand (Object template, int depth,
			SyntaxForm syntax, Object seen, Translator tr)
  {
    /* #ifdef use:java.util.IdentityHashMap */ 
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
		&& tr.matches((pair = (Pair)element).car, syntax,
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
		  args[i] = coerceExpression (buffer[i], tr);
		else if (state[i] < 2)
		  {
		    Object[] arg1 = new Object[1];
		    arg1[0] = buffer[i];
		    args[i] = leaf(new FVector (arg1), tr);
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
    else
      result = template;
    /* #ifdef use:java.util.IdentityHashMap */ 
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
      return tr.syntaxError ("wrong number of arguments to quote");
    return coerceExpression(expand(pair.car, isQuasi ? 1 : QUOTE_DEPTH, tr), tr);
  }

  /** A wrapper around LList.consX to make it a "variable-arg method". */
  public static Object consX$V (Object[] args)
  {
    return LList.consX(args);
  }

  /** Same as regular append, but handle SyntaxForm wrappers. */
  public static Object append$V (Object[] args)
  {
    int count = args.length;
    if (count == 0)
      return LList.Empty;
    Object result = args[count - 1];
    for (int i = count - 1; --i >= 0; )
      {
	Object list = args[i];
	Object copy = null;
	Pair last = null;
        SyntaxForm syntax = null;
        for (;;)
	  {
            while (list instanceof SyntaxForm)
              {
                syntax = (SyntaxForm) list;
                list = syntax.form;
              }
            if (list == LList.Empty)
              break;
	    Pair list_pair = (Pair) list;
            Object car = list_pair.car;
            if (syntax != null && ! (car instanceof SyntaxForm))
              car = SyntaxForm.make(car, syntax.scope);
	    Pair new_pair = new Pair(car, null);
	    if (last == null)
	      copy = new_pair;
	    else
	      last.cdr = new_pair;
	    last = new_pair;
	    list = list_pair.cdr;
	  }
	if (last != null)
	  {
	    last.cdr = result;
	    result = copy;
	  }
      }
    return result;
  }

  static final ClassType vectorType = ClassType.make("kawa.lib.vectors");
  static final ClassType vectorAppendType
    = ClassType.make("kawa.standard.vector_append");
  static final ClassType quoteType = ClassType.make("kawa.lang.Quote");
}

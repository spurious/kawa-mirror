package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/**
 * The Syntax transformer that re-writes the "quasiquote" Scheme primitive.
 * @author	Per Bothner
 */

public class quasiquote extends Syntax implements Printable
{
  private static Expression coerceExpression (Object val)
  {
    return val instanceof Expression ? (Expression) val : new QuoteExp (val);
  }

  Object expand_pair (Pair pair, int depth, Translator tr)
  {
    if (pair.car == Scheme.quasiquote_sym)
      depth++;
    else if (pair.car == Scheme.unquote_sym)
      {
	depth--;
	Pair pair_cdr;
	if (! (pair.cdr instanceof Pair)
	    || (pair_cdr = (Pair) pair.cdr).cdr != LList.Empty)
	  return tr.syntaxError ("invalid used of " + pair.car +
				     " in quasiquote template");
	if (depth == 0)
	  return tr.rewrite (pair_cdr.car);
      }
    else if (pair.car == Scheme.unquotesplicing_sym && depth >= 0)
      return tr.syntaxError ("invalid used of " + pair.car +
				 " in quasiquote template");
    Object expanded_cdr = expand (pair.cdr, depth, tr);
    Pair pair_car;
    if (pair.car instanceof Pair
	&& (pair_car = (Pair)pair.car).car == Scheme.unquotesplicing_sym
	&& --depth == 0)
      {
	Pair pair_car_cdr;
	if (! (pair_car.cdr instanceof Pair)
	    || (pair_car_cdr = (Pair) pair_car.cdr).cdr != LList.Empty)
	  return tr.syntaxError ("invalid used of " + pair_car.car +
				     " in quasiquote template");
	Procedure append = kawa.standard.append.appendProcedure;
	Expression[] args = new Expression[2];
	args[0] = tr.rewrite (pair_car_cdr.car);
	args[1] = coerceExpression (expanded_cdr);
	return new ApplyExp (new QuoteExp (append), args);
      }
    Object expanded_car = expand (pair.car, depth, tr);
    if (expanded_car == pair.car && expanded_cdr == pair.cdr)
      return pair;
    else if (!(expanded_car instanceof Expression)
	     && !(expanded_cdr instanceof Expression))
      return new Pair (expanded_car, expanded_cdr);
    else
      {
	Expression cons = new QuoteExp (kawa.standard.cons.consProcedure);
	Expression[] args = new Expression[2];
	args[0] = coerceExpression (expanded_car);
	args[1] = coerceExpression (expanded_cdr);
	return new ApplyExp (cons, args);
      }
  }

  /** Backquote-expand a template.
   * @param template the quasiquoted template to expand
   * @param depth the (net) number of quasiquotes we are inside
   * @param tr the rewrite context
   * @return the expanded Expression (the result can be a non-expression,
   *   in which case it is implicitly in a QuoteExp).
   */
  Object expand (Object template, int depth, Translator tr)
  {
    if (template instanceof Pair)
      return expand_pair ((Pair) template, depth, tr);
    else if (template instanceof FVector)
      {
	FVector vector = (FVector) template;
	int n = vector.length ();
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
	    Object element = vector.elementAt (i);
	    int element_depth = depth;
	    Pair pair;
	    if (element instanceof Pair
		&& ((pair = (Pair)element).car
		    == Scheme.unquotesplicing_sym)
		&& --element_depth == 0)
	      {
		Pair pair_cdr;
		if (! (pair.cdr instanceof Pair)
		    || (pair_cdr = (Pair) pair.cdr).cdr != LList.Empty)
		  return tr.syntaxError ("invalid used of " + pair.car +
					     " in quasiquote template");
		buffer[i] = tr.rewrite (pair_cdr.car);
		state[i] = 3;
	      }
	    else
	      {
		buffer [i] = expand (element, element_depth, tr);
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
	  return vector;
	if (max_state == 1)
	  return new FVector (buffer);
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
		Procedure func = kawa.standard.vector_v.vectorProcedure;
		args[i] = new ApplyExp (new QuoteExp (func), arg1);
	      }
	  }
	Procedure func;
	if (max_state < 3)
	  func = kawa.standard.vector_v.vectorProcedure;
	else
	  func = kawa.standard.vector_append.vappendProcedure;
	return new ApplyExp (new QuoteExp (func), args);
      }
    else
      return template;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Pair pair;
    if (! (obj instanceof Pair)
	|| (pair = (Pair) obj).cdr != LList.Empty)
      return tr.syntaxError ("wrong number of arguments to quasiquote");
    return coerceExpression (expand (pair.car, 1, tr));
  }
}

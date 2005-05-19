package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.util.*;

public class syntax extends Syntax
{
  public static final syntax syntax = new syntax();
  static { syntax.setName("syntax"); }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    /* #ifdef use:java.util.IdentityHashMap */ 
    IdentityHashMap table = new IdentityHashMap();
    /* #endif */

    if (! (form.cdr instanceof Pair)
	|| (form = (Pair) (form.cdr)).cdr != LList.Empty)
      return tr.syntaxError("syntax forms requires a single form");
    SyntaxTemplate template = new SyntaxTemplate(form.car, null, tr);
    Expression matchArray = QuoteExp.nullExp;
    PatternScope patternScope = tr.patternScope;
    if (patternScope != null && patternScope.matchArray != null)
      matchArray = new ReferenceExp(patternScope.matchArray);
    Expression[] args = { new QuoteExp(template), matchArray };
    return new ApplyExp(ClassType.make("kawa.lang.SyntaxTemplate")
			.getDeclaredMethod("execute", 1),
			args);
  }

  Expression wrap (Object form, Translator tr)
  {
    if (form instanceof Expression)
      return (Expression) form;
    ClassType typeSyntaxForm = ClassType.make("kawa.lang.SyntaxForm");
    Method makeSyntaxFormMethod = typeSyntaxForm.getDeclaredMethod("make", 2);
    Expression[] args = { new QuoteExp(form), new QuoteExp(tr) };
    return new ApplyExp(makeSyntaxFormMethod, args);
  }

  Object convert (Object form, Object m, Translator tr)
  {
    /* #ifndef use:java.util.IdentityHashMap */ 
    // Object map = m;
    /* #endif */
    Object x;
    /* #ifdef use:java.util.IdentityHashMap */ 
    IdentityHashMap map = (IdentityHashMap) m;
    x = map.get(form);
    if (x != null)
      return x;
    map.put(form, form);
    /* #endif */
    x = form;
    if (form instanceof Pair)
      {
	Pair p = (Pair) form;
	Object car = convert(p.car, map, tr);
	Object cdr = convert(p.cdr, map, tr);
	if (car == p.car && cdr == p.cdr)
	  return p;
	Expression[] args = new Expression[2];
	args[0] = wrap(car, tr);
	args[1] = wrap(cdr, tr);
	Method consMethod = Compilation.typePair.getDeclaredMethod("make", 2);
	x = new ApplyExp(consMethod, args);
	/*
	if (p instanceof PairWithPosition)
	  x = new PairWithPosition((PairWithPosition) p, car, cdr);
	else
	  x = Pair(car, cdr);
	*/
      }
    else if (form instanceof FVector)
      {
	FVector v = (FVector) form;
	int len = v.size();
	Object[] r = new Object[len];
	boolean same = true;
	for (int i = 0;  i < len;  i++)
	  {
	    Object vi = v.get(i);
	    Object ri = convert(vi, map, tr);
	    if (ri != vi)
	      same = false;
	  }
	if (same)
	  return form;
      }
    else if (form instanceof String || form instanceof Symbol)
      {
	Declaration d = tr.lexical.lookup(form, Language.VALUE_NAMESPACE);
	if (d != null)
	  x = new ReferenceExp(d);
      }
    /* #ifdef use:java.util.IdentityHashMap */ 
    map.put(form, x);
    /* #endif */
    return x;
  }
}

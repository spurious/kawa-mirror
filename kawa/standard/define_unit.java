// Copyright (C) 2000 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.expr.*;
import gnu.math.*;
import gnu.bytecode.*;

public class define_unit extends Syntax
{
  /** True if this is define-base-unit, false if define-unit. */
  boolean base;

  public define_unit (boolean base)
  {
    this.base = base;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (st.cdr instanceof Pair)
      {
	Pair p = (Pair) st.cdr;
	Object q = p.car;
	if (q instanceof String)
	  {
	    String name = (String) q;
	    String sym = (name + "$unit").intern();
	    Declaration decl = defs.getDefine(sym, 'w', tr);
	    Translator.setLine(decl, p);
	    decl.setFlag(Declaration.IS_CONSTANT);
	    if (defs instanceof ModuleExp)
	      decl.setCanRead(true);
	    Unit unit = null;
	    if (base && p.cdr == LList.Empty)
	      unit = BaseUnit.make(name, (String) null);
	    else if (p.cdr instanceof Pair)
	      {
		Object v = ((Pair) p.cdr).car;
		if (base && v instanceof FString)
		  unit = BaseUnit.make(name, v.toString());
		else if (! base && v instanceof Quantity)
		  unit = Unit.make(name, (Quantity) v);
	      }
	    if (unit != null)
	      {
		// Add to translation environment, so read will find it.
		tr.addGlobal(name, unit);
		decl.noteValue(new QuoteExp(unit));
	      }
	    p = tr.makePair(p, decl, p.cdr);
	    st = tr.makePair(st, this, p);
	    forms.addElement (st);
	    return true;
	  }
      }
    tr.error('e', "missing name in define-unit");
    return false;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Expression value = null;
    Pair p1;

    if (! (obj instanceof Pair)
	|| ! ((p1 = (Pair) obj).car instanceof Declaration))
      return tr.syntaxError ("invalid syntax for "+getName());
    Declaration decl = (Declaration) p1.car;
    String name = decl.getName();
    String unit = name.substring(0, name.length() - 5).intern(); // Drop $unit.
    ClassType unitType = ClassType.make("gnu.math.Unit");
    decl.setType(unitType);
    if ((value = decl.getValue()) instanceof QuoteExp
	&& ((QuoteExp) value).getValue() instanceof Unit)
      ;
    else if (base)
      {
	String dimension = null;
	if (p1.cdr != LList.Empty)
	  {
	    dimension = ((Pair) p1.cdr).car.toString();
	  }
	BaseUnit bunit = BaseUnit.make(unit, dimension);
	value = new QuoteExp(bunit);
      }
    else
      {
	if (! (p1.cdr instanceof Pair))
	  return tr.syntaxError("missing value for define-unit");
	Pair p2 = (Pair) p1.cdr;
	value = tr.rewrite (p2.car);
	Object quantity;
	if (value instanceof QuoteExp
	    && (quantity = ((QuoteExp) value).getValue()) instanceof Quantity)
	  {
	    tr.addGlobal(name, quantity);  // Add to translation environment.

	    value = new QuoteExp(Unit.make(unit, (Quantity) quantity));
	  }
	else
	  {
	    Expression[] args = new Expression[2];
	    args[0] = new QuoteExp(unit);
	    args[1] = value;
	    value = gnu.kawa.reflect.Invoke.makeInvokeStatic(unitType, "make",
							     args);
	  }
      }
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    sexp.binding = decl;
    decl.noteValue(value);
    return sexp;
  }
}

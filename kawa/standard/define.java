package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;

/**
 * The Syntax transformer that re-writes the "define" Scheme primitive.
 * @author	Per Bothner
 */

public class define extends Syntax implements Printable
{
  boolean makePrivate;
  boolean makeConstant;
  public define(boolean makePrivate)
  {
    this.makePrivate = makePrivate;
  }
  public define(boolean makePrivate, boolean makeConstant)
  {
    this.makePrivate = makePrivate;
    this.makeConstant = makeConstant;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    Pair namePair = p;
    String sym = null;
    boolean function = false;
    if (name instanceof String)
      {
	sym = (String) name;
      }
    else if (name instanceof Pair)
      {
        namePair = (Pair) name;
        if (namePair.car instanceof String)
	  sym = (String) namePair.car;
	function = true;
      }
    if (sym != null)
      {
	Declaration decl = defs.getDefine(sym, 'w', tr);
	if (makePrivate)
	  decl.setPrivate(true);
	if (makeConstant)
	  decl.setFlag(Declaration.IS_CONSTANT);
	Object declForm = (! function) ? (Object) decl
	  : (Object) tr.makePair(namePair, decl, namePair.cdr);
	p = tr.makePair(p, declForm, p.cdr);
	st = tr.makePair(st, this, p);
        if (defs instanceof ModuleExp)
          {
            if (! makePrivate)
              {
                decl.setCanRead(true);
		// (define (f) ...) defaults f to being read-only,
		// unless f is assigned to in this module.
		if (! makeConstant
		    && (name instanceof String || Compilation.usingTailCalls))
		  decl.setCanWrite(true);
              }
          }
	Translator.setLine(decl, namePair);
      }
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    String name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.car instanceof String && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		name = (String) p1.car;
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Declaration && p1.cdr instanceof Pair)
	  {
	    decl = (Declaration) p1.car;
	    Pair p2 = (Pair) p1.cdr;
	    Pair p3;
	    if ("::" == p2.car && p2.cdr instanceof Pair
		&& (p3 = (Pair) p2.cdr).cdr instanceof Pair)
	      {
		decl.setType(kawa.standard.prim_method.exp2Type(p3.car, tr));
		decl.setFlag(Declaration.TYPE_SPECIFIED);
		p2 = (Pair) p3.cdr;
	      }
	    if (p2.cdr == LList.Empty)
	      {
                name = decl.getName();
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.car;
	    if (p2.car instanceof String)
	      {
		name = (String) p2.car;
              }
            else if (p2.car instanceof Declaration)
              {
                decl = (Declaration) p2.car;
                name = decl.getName();
              }
            if (name != null)
              {
		LambdaExp lexp = new LambdaExp();
		Lambda.rewrite(lexp, p2.cdr, p1.cdr, tr);
		lexp.setName (name);
		if (p2 instanceof PairWithPosition)
		  {
		    PairWithPosition pp = (PairWithPosition) p2;
		    lexp.setFile (pp.getFile ());
		    lexp.setLine (pp.getLine (), pp.getColumn ());
		  }
		value = lexp;
	      }
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    if (decl != null)
      {
	sexp.binding = decl;
	if (decl.context instanceof ModuleExp && ! makePrivate
	    && decl.getCanWrite())
	  value = null;
	decl.noteValue(value);
      }
    if (! (tr.currentScope() instanceof ModuleExp))
      {
        if (makePrivate)
          {
            tr.error('w', "define-private not at top level "
                     +tr.currentScope());
            return sexp;
          }
      }
    return sexp;
  }
}

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
  public define(boolean makePrivate)
  {
    this.makePrivate = makePrivate;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p = (Pair) st.cdr;
    Object name = p.car;
    Pair name_pair = null;
    String sym = null;
    if (name instanceof String)
      {
	sym = (String) name;
      }
    else if (name instanceof Pair)
      {
        name_pair = (Pair) name;
        if (name_pair.car instanceof String)
	  sym = (String) name_pair.car;
      }
    if (sym != null)
      {
	Declaration decl = defs.lookup(sym);
	if (decl == null)
	  {
	    decl = new Declaration(sym);
	    defs.addDeclaration(decl);
	  }
	else
	  tr.error('w', "duplicate declaration for `"+sym+"'");
	Object declForm = name_pair == null ? decl
	  : declForm = tr.makePair(name_pair, decl, name_pair.cdr);
	p = tr.makePair(p, declForm, p.cdr);
	st = tr.makePair(st, this, p);
        if (defs instanceof ModuleExp)
          {
            if (! makePrivate)
              {
                decl.setCanRead(true);
		// (define (f) ...) defaults f to being read-only,
		// unless f is assigned to in this module.
		if (name instanceof String || Compilation.usingTailCalls)
		  decl.setCanWrite(true);
              }
          }
        if (declForm instanceof PairWithPosition)
          {
            PairWithPosition declPos = (PairWithPosition) declForm;
            decl.setFile(declPos.getFile());
            decl.setLine(declPos.getLine(), declPos.getColumn());
          }
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
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		decl = (Declaration) p1.car;
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
    if (makePrivate)
      decl.setPrivate(true);
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

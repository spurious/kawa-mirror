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
    Declaration decl = null;
    Pair declForm = null;
    if (name instanceof String)
      {
        decl = new Declaration((String) name);
        declForm = tr.makePair(p, decl, p.cdr);
        st = tr.makePair(st, this, declForm);
      }
    else if (name instanceof Pair)
      {
        Pair name_pair = (Pair) name;
        if (name_pair.car instanceof String)
          {
            decl = new Declaration((String) name_pair.car);
            declForm = tr.makePair(name_pair, decl, name_pair.cdr);
            p = tr.makePair(p, declForm, p.cdr);
            st = tr.makePair(st, this, p);
          }
      }
    if (decl != null)
      {
        if (defs instanceof ModuleExp)
          {
            tr.mustCompileHere();
            tr.push(decl);
            if (! makePrivate)
              {
                decl.setCanRead(true);
                // decl.setCanWrite(true);
              }
          }
        if (declForm instanceof PairWithPosition)
          {
            PairWithPosition declPos = (PairWithPosition) declForm;
            decl.setFile(declPos.getFile());
            decl.setLine(declPos.getLine(), declPos.getColumn());
          }
        defs.addDeclaration(decl);
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
	decl.noteValue (value);
      }
    if (! (tr.currentScope() instanceof ModuleExp))
      {
        if (makePrivate)
          {
            tr.error('w', "define-private not at top level "
                     +tr.currentScope());
            return sexp;
          }

        /*
        // FIXME:  Remove following once define is handled like define-private.

	Object binding = tr.current_decls.get (name);
	// Hygenic macro expansion may bind a renamed (uninterned) symbol
	// to the original symbol.
	if (binding == null || binding instanceof String)
	  return tr.syntaxError ("invalid use of define");
	sexp.binding = (Declaration) binding;
	sexp.binding.noteValue (value);
        */
      }
    return sexp;
  }
}

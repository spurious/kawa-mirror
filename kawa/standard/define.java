package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/**
 * The Syntax transformer that re-writes the "define" Scheme primitive.
 * @author	Per Bothner
 */

public class define extends Syntax implements Printable
{
  Lambda lambda;

  boolean makePrivate;
  boolean makeConstant;
  public define(Lambda lambda, boolean makePrivate)
  {
    this.lambda = lambda;
    this.makePrivate = makePrivate;
  }
  public define(Lambda lambda, boolean makePrivate, boolean makeConstant)
  {
    this.lambda = lambda;
    this.makePrivate = makePrivate;
    this.makeConstant = makeConstant;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    if (! (st.cdr instanceof Pair))
      return super.scanForDefinitions(st, forms, defs, tr);
    Pair p1 = (Pair) st.cdr;
    Object name = p1.car;
    Pair namePair = p1;
    Object sym = null;
    LambdaExp lexp = null;
    if ((name instanceof String || name instanceof Symbol)
	&& p1.cdr instanceof Pair)
      {
	sym = name;
      }
    else if (name instanceof Pair)
      {
        namePair = (Pair) name;
        if (namePair.car instanceof String
	    || namePair.car instanceof Symbol)
	  sym = namePair.car;
	lexp = new LambdaExp();
      }
    if (sym != null)
      {
	Declaration decl = defs.getDefine(sym, 'w', tr);
	tr.push(decl);
	if (makePrivate)
	  {
	    decl.setFlag(Declaration.PRIVATE_SPECIFIED);
	    decl.setPrivate(true);
	  }
	if (makeConstant)
	  decl.setFlag(Declaration.IS_CONSTANT);
	Object declForm;
	if (lexp != null)
	  {
	    decl.setProcedureDecl(true);
	    lexp.nameDecl = decl;
	    lambda.rewrite(lexp, namePair.cdr, tr);
	    declForm = tr.makePair(namePair, lexp, namePair.cdr);
	    declForm = tr.makePair(p1, declForm,
				   lambda.rewriteAttrs(lexp, p1.cdr, tr));
	  }
	else
	  {
	    Pair p2 = (Pair) p1.cdr;
	    Pair p3;
	    if (tr.matches(p2.car, "::") && p2.cdr instanceof Pair
		&& (p3 = (Pair) p2.cdr).cdr instanceof Pair)
	      {
		decl.setType(tr.exp2Type(p3));
		decl.setFlag(Declaration.TYPE_SPECIFIED);
		p2 = (Pair) p3.cdr;
	      }
	    Object value_list;
	    if (p2.cdr == LList.Empty)
	      {
                name = decl.getSymbol();
		value_list = p2;
	      }
	    else
	      value_list = p2.cdr;
	    declForm = tr.makePair(p1, decl, value_list);
	  }
        if (defs instanceof ModuleExp)
          {
	    if (! makePrivate)
              {
                decl.setCanRead(true);
		// (define (f) ...) defaults f to being read-only,
		// unless f is assigned to in this module.
		if (! makeConstant
		    && (name instanceof String
			|| ! Compilation.inlineOk))
		  decl.setCanWrite(true);
              }
          }
	st = tr.makePair(st, this, declForm);
	Translator.setLine(decl, namePair);
      }
    forms.addElement (st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.cdr;
    Object name = null;
    Expression value = null;
    Declaration decl = null;
    boolean not_in_body = false;
    LambdaExp lexp = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if ((p1.car instanceof String
	     || p1.car instanceof Symbol)
	    && p1.cdr instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.cdr;
	    if (p2.cdr == LList.Empty)
	      {
		name = p1.car;
		not_in_body = true;
	      }
	  }
	else if (p1.car instanceof Declaration && p1.cdr instanceof Pair)
	  {
	    decl = (Declaration) p1.car;
	    Pair p2 = (Pair) p1.cdr;
	    Pair p3;
	    if (p2.cdr == LList.Empty)
	      {
                name = decl.getSymbol();
		value = tr.rewrite (p2.car);
	      }
	  }
	else if (p1.car instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.car;
	    if (p2.car instanceof String || p2.car instanceof Symbol)
	      {
		name = p2.car;
		not_in_body = true;
              }
            else if (p2.car instanceof LambdaExp)
              {
		lexp = (LambdaExp) p2.car;
                decl = lexp.nameDecl;
		lexp.nameDecl = null;
                name = decl.getSymbol();

		if (p1.cdr instanceof PairWithPosition)
		  lexp.setFile(((PairWithPosition) p1.cdr).getFile());
		lambda.rewriteBody(lexp, p1.cdr, tr);
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
    if (not_in_body)
      return tr.syntaxError (getName() + " is only allowed in a <body>");
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    sexp.binding = decl;
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

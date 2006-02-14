package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/**
 * The Syntax transformer that re-writes the "%define" internal form.
 * This is used to implement define, define-private, and define-constant.
 * Syntax: <code>(%define name code type value)</code>.
 * The <code>name</code> is an identifier (<code>String</code> or
 * <code>Symbol</code>) or </code>Declaration</code>.
 * The <code>code</code> is an integer mask,
 * where 1 means type specified, 2 means a function definition,
 * 4 means private, and 8 means constant.
 * The <code>type</code> is the declarated type or <code>null</code>.
 * The <code>value</code> is the initializing value. * 
 * @author	Per Bothner
 */

public class define extends Syntax implements Printable
{
  public static final define defineRaw = new define(Scheme.lambda);

  Lambda lambda;

  String getName (int options)
  {
    if ((options & 4) != 0)
      return "define-private";
    else if ((options & 8) != 0)
      return "define-constant";
    else
      return "define";
  }

  public define(Lambda lambda)
  {
    this.lambda = lambda;
  }

  public void scanForm (Pair st, ScopeExp defs, Translator tr)
  {
    Pair p1 = (Pair) st.cdr;
    Pair p2 = (Pair) p1.cdr;
    Pair p3 = (Pair) p2.cdr;
    Pair p4 = (Pair) p3.cdr;
    SyntaxForm nameSyntax = null;
    Object name = p1.car;
    while (name instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) name;
	name = nameSyntax.form;
      }
    int options = ((Number) Translator.stripSyntax(p2.car)).intValue();
    boolean makePrivate = (options & 4) != 0;
    boolean makeConstant = (options & 8) != 0;

    ScopeExp scope = tr.currentScope();
    name = tr.namespaceResolve(name);
    if (! (name instanceof String || name instanceof Symbol))
      {
        tr.error('e', "'"+name+"' is not a valid identifier");
        name = null;
      }

    Object savePos = tr.pushPositionOf(p1);
    Declaration decl = tr.define(name, nameSyntax, defs);
    tr.popPositionOf(savePos);
    name = decl.getSymbol();
    if (makePrivate)
      {
	decl.setFlag(Declaration.PRIVATE_SPECIFIED);
	decl.setPrivate(true);
      }
    if (makeConstant)
      decl.setFlag(Declaration.IS_CONSTANT);

    if ((options & 2) != 0)
      {
	LambdaExp lexp = new LambdaExp();
	decl.setProcedureDecl(true);
        decl.setType(Compilation.typeProcedure);
	lexp.setSymbol(name);
	lexp.nameDecl = decl;
	Object formals = p4.car;
	Object body = p4.cdr;
	Translator.setLine(lexp, p1);
	lambda.rewriteFormals(lexp, formals, tr, null);
	Object realBody = lambda.rewriteAttrs(lexp, body, tr);
	if (realBody != body)
	  p2 = new Pair(p2.car, new Pair(p3.car, new Pair(formals, realBody)));
	decl.noteValue(lexp);
      }

    if (defs instanceof ModuleExp)
      {
	if (! makePrivate)
	  {
	    decl.setCanRead(true);
	    // (define (f) ...) defaults f to being read-only,
	    // unless f is assigned to in this module.
	    if (! makeConstant
		&& ((options & 2) != 0
		    || ! Compilation.inlineOk))
	      decl.setCanWrite(true);
	  }
      }

    if ((options & 1) != 0)
      {
	decl.setType(tr.exp2Type(p3));
	decl.setFlag(Declaration.TYPE_SPECIFIED);
      }

    st = Translator.makePair(st, this,
			     Translator.makePair(p1, decl, p2));
    Translator.setLine(decl, p1);

    tr.formStack.addElement(st);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair p1 = (Pair) form.cdr;
    Pair p2 = (Pair) p1.cdr;
    Pair p3 = (Pair) p2.cdr;
    Pair p4 = (Pair) p3.cdr;
    Object name = Translator.stripSyntax(p1.car);
    int options = ((Number) Translator.stripSyntax(p2.car)).intValue();
    boolean makePrivate = (options & 4) != 0;
    boolean makeConstant = (options & 8) != 0;

    if (! (name instanceof Declaration))
      return tr.syntaxError(getName(options) + " is only allowed in a <body>");
    Declaration decl = (Declaration) name;

    Expression value;
    if ((options & 2) != 0)
      {
	LambdaExp lexp = (LambdaExp) decl.getValue();
	Object body = p4.cdr;
	lambda.rewriteBody(lexp, body, tr);
	value = lexp;
      }
    else
      {
	value = tr.rewrite (p4.car);
	decl.noteValue((decl.context instanceof ModuleExp && ! makePrivate
			&& decl.getCanWrite())
		       ? null : value);
      }
    SetExp sexp = new SetExp(decl, value);
    sexp.setDefining (true);
    if (makePrivate && ! (tr.currentScope() instanceof ModuleExp))
      tr.error('w', "define-private not at top level "
	       +tr.currentScope());
    return sexp;
  }
}

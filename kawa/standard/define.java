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

public class define extends Syntax
{
  public static final define defineRaw = new define(SchemeCompilation.lambda);

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
    Pair p1 = (Pair) st.getCdr();
    Pair p2 = (Pair) p1.getCdr();
    Pair p3 = (Pair) p2.getCdr();
    Pair p4 = (Pair) p3.getCdr();
    SyntaxForm nameSyntax = null;
    Object name = p1.getCar();
    while (name instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) name;
	name = nameSyntax.getDatum();
      }
    int options = ((Number) Translator.stripSyntax(p2.getCar())).intValue();
    boolean makePrivate = (options & 4) != 0;
    boolean makeConstant = (options & 8) != 0;

    ScopeExp scope = tr.currentScope();
    name = tr.namespaceResolve(name);
    if (! (name instanceof Symbol))
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
    decl.setFlag(Declaration.IS_SINGLE_VALUE);

    if ((options & 2) != 0)
      {
	LambdaExp lexp = new LambdaExp();
	lexp.setSymbol(name);
        if (Compilation.inlineOk)
          {
            decl.setProcedureDecl(true);
            decl.setType(Compilation.typeProcedure);
            lexp.nameDecl = decl;
          }
	Object formals = p4.getCar();
	Object body = p4.getCdr();
	Translator.setLine(lexp, p1);
	lambda.rewriteFormals(lexp, formals, tr, null);
	Object realBody = lambda.rewriteAttrs(lexp, body, tr);
	if (realBody != body)
	  p2 = new Pair(p2.getCar(), new Pair(p3.getCar(), new Pair(formals, realBody)));
	decl.noteValue(lexp);
      }

    if (defs instanceof ModuleExp && ! makePrivate
        && (! Compilation.inlineOk || tr.sharedModuleDefs()))
      decl.setCanWrite(true);

    if ((options & 1) != 0)
      {
        decl.setTypeExp(new LangExp(p3));
	decl.setFlag(Declaration.TYPE_SPECIFIED);
      }

    st = Translator.makePair(st, this,
			     Translator.makePair(p1, decl, p2));
    Translator.setLine(decl, p1);

    tr.formStack.addElement(st);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair p1 = (Pair) form.getCdr();
    Pair p2 = (Pair) p1.getCdr();
    Pair p3 = (Pair) p2.getCdr();
    Pair p4 = (Pair) p3.getCdr();
    Object name = Translator.stripSyntax(p1.getCar());
    int options = ((Number) Translator.stripSyntax(p2.getCar())).intValue();
    boolean makePrivate = (options & 4) != 0;

    if (! (name instanceof Declaration))
      return tr.syntaxError(getName(options) + " is only allowed in a <body>");
    Declaration decl = (Declaration) name;

    if (decl.getFlag(Declaration.TYPE_SPECIFIED))
      {
        Expression texp = decl.getTypeExp();
        if (texp instanceof LangExp)
          {
            Pair typeSpecPair = (Pair) ((LangExp) texp).getLangValue(); 
            decl.setType(tr.exp2Type(typeSpecPair));
          }
      }

    Expression value;
    if ((options & 2) != 0)
      {
	LambdaExp lexp = (LambdaExp) decl.getValue();
	Object body = p4.getCdr();
	lambda.rewriteBody(lexp, body, tr);
	value = lexp;
        if (! Compilation.inlineOk)
          decl.noteValue(null);
      }
    else
      {
	value = tr.rewrite (p4.getCar());
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

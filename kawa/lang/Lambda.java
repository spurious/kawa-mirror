package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.Type;
import gnu.kawa.functions.Convert;

/**
 * The Syntax transformer that re-writes the lambda builtin.
 * @author	Per Bothner
 */

public class Lambda extends Syntax implements Printable
{
  public Object optionalKeyword;
  public Object restKeyword;
  public Object keyKeyword;

  public Expression defaultDefault = QuoteExp.falseExp;

  public void setKeywords(Object optional, Object rest, Object key)
  {
    optionalKeyword = optional;
    restKeyword = rest;
    keyKeyword = key;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing formals in lambda");
    int old_errors = tr.getMessages().getErrorCount();
    LambdaExp lexp = new LambdaExp();
    Pair pair = (Pair) obj;
    Translator.setLine(lexp, pair);
    rewrite(lexp, pair.car, pair.cdr, tr, null);
    if (tr.getMessages().getErrorCount() > old_errors)
      return new ErrorExp("bad lambda expression");
    return lexp;
  }

  /**
   * Higher-level constructor, that does the re-writing.
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param tr the (Scheme) Translator
   */
  public void rewrite(LambdaExp lexp, Object formals, Object body,
		      Translator tr, TemplateScope templateScopeRest)
  {
    rewriteFormals(lexp, formals, tr, templateScopeRest);
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());
    body = rewriteAttrs(lexp, body, tr);
    rewriteBody(lexp, body, tr);
  }

  public void rewriteFormals(LambdaExp lexp, Object formals,
		      Translator tr, TemplateScope templateScopeRest)
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    int opt_args = -1;
    int rest_args = -1;
    int key_args = -1;
    Pair pair;
    for (; ;  bindings = pair.cdr)
      {
	if (bindings instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) bindings;
	    // FIXME
	    bindings = sf.form;
	  }
	if (! (bindings instanceof Pair))
	  break;
	pair = (Pair) bindings;
        // An initial pass to count the parameters.
	Object pair_car = pair.car;
	if (pair_car instanceof SyntaxForm)
	  pair_car = ((SyntaxForm) pair_car).form;
	if (pair_car == optionalKeyword)
	  {
	    if (opt_args >= 0)
	      {
		tr.syntaxError ("multiple "+optionalKeyword+" in parameter list");
		return;
	      }
	    else if (rest_args >= 0 || key_args >= 0)
	      {
		tr.syntaxError (optionalKeyword.toString()+" after " + restKeyword + " or " + keyKeyword);
		return;
	      }
	    opt_args = 0;
	  }
	else if (pair_car == restKeyword)
	  {
	    if (rest_args >= 0)
	      {
		tr.syntaxError ("multiple " + restKeyword
                                + " in parameter list");
		return;
	      }
	    else if (key_args >= 0)
	      {
		tr.syntaxError (restKeyword.toString()
                                + " after " + keyKeyword);
		return;
	      }
	    rest_args = 0;
	  }
	else if (pair_car == keyKeyword)
	  {
	    if (key_args >= 0)
	      {
		tr.syntaxError ("multiple " + keyKeyword
                                + " in parameter list");
		return;
	      }
	    key_args = 0;
	  }
        else if (tr.matches(pair.car, "::") && pair.cdr instanceof Pair)
          pair = (Pair) pair.cdr;
	else if (key_args >= 0)
	  key_args++;
	else if (rest_args >= 0)
	  rest_args++;
	else if (opt_args >= 0)
	  opt_args++;
	else
	  lexp.min_args++;
	bindings = pair.cdr;
      }
    if (bindings instanceof String || bindings instanceof Symbol)
      {
	if (opt_args >= 0 || key_args >= 0 || rest_args >= 0)
	  {
	    tr.syntaxError ("dotted rest-arg after " + optionalKeyword
                            +", " + restKeyword + ", or " + keyKeyword);
	    return;
	  }
	rest_args = 1;
      }
    else if (bindings != LList.Empty)
      {
	tr.syntaxError ("misformed formals in lambda");
	return;
      }
    if (rest_args > 1)
      {
	tr.syntaxError ("multiple " + restKeyword + " parameters");
        return;
      }
    if (opt_args < 0)
      opt_args = 0;
    if (rest_args < 0)
      rest_args = 0;
    if (key_args < 0)
      key_args = 0;
    if (rest_args > 0)
      lexp.max_args = -1;
    else   // Is this useful?
      lexp.max_args = lexp.min_args + opt_args + 2 * key_args;
    if (opt_args + key_args > 0)
      lexp.defaultArgs = new Expression[opt_args + key_args];
    if (key_args > 0)
      lexp.keywords = new Keyword[key_args];

    bindings = formals;
    int i = 0;
    opt_args = 0;
    key_args = 0;
    Object mode = null;
    for (; ;  bindings = pair.cdr)
      {
	if (bindings instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) bindings;
	    bindings = sf.form;
	    // The SyntaxForm "surrounds" both the current binding (the car),
	    // as well as the cdr - i.e. the remaining bindings.
	    templateScopeRest = sf.scope;
	  }
	TemplateScope templateScope = templateScopeRest;
	if (! (bindings instanceof Pair))
	  break;
	pair = (Pair) bindings;
	Object pair_car = pair.car;
	if (pair_car instanceof SyntaxForm)
	  {
	    SyntaxForm sf = (SyntaxForm) pair_car;
	    pair_car = sf.form;
	    templateScope = sf.scope;
	  }
	if (pair_car == optionalKeyword
	    || pair_car == restKeyword || pair_car == keyKeyword)
	  {
	    mode = pair_car;
	    continue;
	  }
	Object savePos = tr.pushPositionOf(pair);
	Object name = null;
	Object defaultValue = defaultDefault;
	Pair typeSpecPair = null;
        Pair p;
	if (tr.matches(pair_car, "::"))
	  {
	    tr.syntaxError("'::' must follow parameter name");
	    return;
	  }
	if (pair_car instanceof String || pair_car instanceof Symbol)
          {
            name = pair_car;
            if (pair.cdr instanceof Pair
                && tr.matches((p = (Pair) pair.cdr).car, "::"))
              {
                if (! (pair.cdr instanceof Pair))
                  {
                    tr.syntaxError("'::' not followed by a type specifier"
                                   + " (for parameter '" + name + "')");
                    return;
                  }
                p = (Pair) p.cdr;
                typeSpecPair = p;
                pair = p;
              }
          }
	else if (pair_car instanceof Pair)
	  {
	    p = (Pair) pair_car;
	    pair_car = p.car;
	    if (pair_car instanceof SyntaxForm)
	      {
		SyntaxForm sf = (SyntaxForm) pair_car;
		pair_car = sf.form;
		templateScope = sf.scope;
	      }
	    if ((pair_car instanceof String
		 || pair_car instanceof Symbol)
		&& p.cdr instanceof Pair)
	      {
		name = pair_car;
		p = (Pair) p.cdr;
		if (tr.matches(p.car, "::"))
		  {
		    if (! (p.cdr instanceof Pair))
		      {
			tr.syntaxError("'::' not followed by a type specifier"
				       + " (for parameter '" + name + "')");
			return;
		      }
		    p = (Pair) p.cdr;
		    typeSpecPair = p;
		    if (p.cdr instanceof Pair)
		      p = (Pair) p.cdr;
		    else if (p.cdr == LList.Empty)
		      p = null;
		    else
		      {
			tr.syntaxError("improper list in specifier for parameter '"
				       + name + "')");
			return;
		      }
		  }
		if (p != null && mode != null)
		  {
		    defaultValue = p.car;
		    if (p.cdr instanceof Pair)
		      p = (Pair) p.cdr;
		    else if (p.cdr == LList.Empty)
		      p = null;
		    else
		      {
			tr.syntaxError("improper list in specifier for parameter '"
				       + name + "')");
			return;
		      }
		  }
		if (p != null)
		  {
		    if (typeSpecPair != null)
		      {
			tr.syntaxError("duplicate type specifier for parameter '"
				       + name + '\'');
			return;
		      }
		    typeSpecPair = p;
		    if (p.cdr != LList.Empty)
		      {
			tr.syntaxError("junk at end of specifier for parameter '"
				       + name + '\''+" after type "+p.car);
			return;
		      }
		  }
	      }
	  }
	if (name == null)
	  {
	    tr.syntaxError ("parameter is neither name nor (name :: type) nor (name default)"+": "+pair);
	    return;
	  }
	if (mode == optionalKeyword || mode == keyKeyword)
	  lexp.defaultArgs[opt_args++] = new LangExp(defaultValue);
	if (mode == keyKeyword)
	  lexp.keywords[key_args++]
	    = Keyword.make(name instanceof Symbol ? ((Symbol) name).getName()
			   : name.toString());
	Declaration decl = new Declaration(name);
	Translator.setLine(decl, bindings);
	if (typeSpecPair != null)
	  {
	    decl.setType(tr.exp2Type(typeSpecPair));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	  }
	else if (mode == restKeyword)
	  decl.setType(Compilation.scmListType);
	decl.noteValue(null);  // Does not have a known value.
	addParam(decl, templateScope, lexp, tr);
	tr.popPositionOf(savePos);
      }
    if (bindings instanceof SyntaxForm)
      {
	SyntaxForm sf = (SyntaxForm) bindings;
	bindings = sf.form;
	templateScopeRest = sf.scope;
      }
    if (bindings instanceof String || bindings instanceof Symbol)
      {
	Declaration decl = new Declaration(bindings);
	decl.setType(Compilation.scmListType);
	decl.noteValue (null);  // Does not have a known value.
	addParam(decl, templateScopeRest, lexp, tr);
      }
  }

  private static void addParam (Declaration decl, ScopeExp templateScope,
				LambdaExp lexp, Translator tr)
  {
    if (templateScope != null)
      decl = tr.makeRenamedAlias(decl, templateScope);
    lexp.addDeclaration(decl);
    if (templateScope != null)
      decl.context = templateScope;
  }

  public Object rewriteAttrs(LambdaExp lexp, Object body, Translator tr)
  {
    String accessFlagName = null;
    String allocationFlagName = null;
    int accessFlag = 0;
    int allocationFlag = 0;
    SyntaxForm syntax = null;
    for (;;)
      {
	while (body instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) body;
	    body = syntax.form;
	  }
	if (! (body instanceof Pair))
	  break;
	Pair pair1 = (Pair) body;
	Object attrName = Translator.stripSyntax(pair1.car);
	Object pair1_cdr = pair1.cdr;
	while (pair1_cdr instanceof SyntaxForm)
	  {
	    syntax = (SyntaxForm) pair1_cdr;
	    pair1_cdr = syntax.form;
	  }
	if (! (pair1_cdr instanceof Pair))
	  break;
	Pair pair2 = (Pair) pair1_cdr;

	Object attrValue;
	if (tr.matches(attrName, "::"))
	  attrName = null;
	else if (! (attrName instanceof Keyword))
	  break;
	if (attrName == null)
	  {
	    Expression attrExpr = tr.rewrite_car(pair2, syntax);
	    gnu.bytecode.Type rtype = tr.getLanguage().getTypeFor(attrExpr);
	    if (rtype != null)
	      lexp.setReturnType(rtype);
	  }
	else if (attrName == kawa.standard.object.accessKeyword)
	  {
	    Expression attrExpr = tr.rewrite_car(pair2, syntax);
	    if (! (attrExpr instanceof QuoteExp)
		|| ! ((attrValue = ((QuoteExp) attrExpr).getValue()) instanceof String
		      || attrValue instanceof FString))
	      tr.error('e', "access: value not a constant symbol or string");
	    else if (lexp.nameDecl == null)
	      tr.error('e', "access: not allowed for anonymous function");
	    else
	      {
		String value = attrValue.toString();
		if ("private".equals(value))
		  accessFlag = Declaration.PRIVATE_ACCESS;
		else if ("protected".equals(value))
		  accessFlag = Declaration.PROTECTED_ACCESS;
		else if ("public".equals(value))
		  accessFlag = Declaration.PUBLIC_ACCESS;
		else if ("package".equals(value))
		  accessFlag = Declaration.PACKAGE_ACCESS;
		else
		  tr.error('e', "unknown access specifier");
		if (accessFlagName != null && value != null)
		  {
		    tr.error('e', "duplicate access specifiers - "
			     + accessFlagName + " and "
			     + value);
		  }
		accessFlagName = value;
	      }
	  }
	else if (attrName == kawa.standard.object.allocationKeyword)
	  {
	    Expression attrExpr = tr.rewrite_car(pair2, syntax);
	    if (! (attrExpr instanceof QuoteExp)
		|| ! ((attrValue = ((QuoteExp) attrExpr).getValue()) instanceof String
		      || attrValue instanceof FString))
	      tr.error('e', "allocation: value not a constant symbol or string");
	    else if (lexp.nameDecl == null)
	      tr.error('e', "allocation: not allowed for anonymous function");
	    else
	      {
		String value = attrValue.toString();
		if ("class".equals(value) || "static".equals(value))
		  allocationFlag = Declaration.STATIC_SPECIFIED;
		else if ("instance".equals(value))
		  allocationFlag = Declaration.NONSTATIC_SPECIFIED;
		else
		  tr.error('e', "unknown allocation specifier");
		if (allocationFlagName != null && value != null)
		  {
		    tr.error('e', "duplicate allocation specifiers - "
			     + allocationFlagName + " and "
			     + value);
		  }
		allocationFlagName = value;
	      }
	  }
	else if (attrName == kawa.standard.object.throwsKeyword)
	  {
	    attrValue = pair2.car;
	    int count = Translator.listLength(attrValue);
	    if (count < 0)
	      tr.error('e', "throws: not followed by a list");
	    else
	      {
		ReferenceExp[] exps = new ReferenceExp[count];
		SyntaxForm syntaxLocal = syntax;
		for (int i = 0;  i < count; i++)
		  {
		    while (attrValue instanceof SyntaxForm)
		      {
			syntaxLocal = (SyntaxForm) attrValue;
			attrValue = syntaxLocal.form;
		      }
		    Pair pair3 = (Pair) attrValue;
		    Expression throwsExpr = tr.rewrite_car(pair3, syntaxLocal);
		    if (throwsExpr instanceof ReferenceExp)
		      {
			exps[i] = (ReferenceExp) throwsExpr;
		      }
		    else
		      {
			Object savePos = tr.pushPositionOf(pair3);
			tr.error('e', "throws not followed by a classname");
			tr.popPositionOf(savePos);
		      }
		    attrValue = pair3.cdr; 
		  }
		lexp.setExceptions(exps);
	      }
	  }
	else
	  {
	    tr.error('w', "unknown procedure property "+attrName);
	  }
	body = pair2.cdr;
      }
    accessFlag |= allocationFlag;
    if (accessFlag != 0)
      lexp.nameDecl.setFlag(accessFlag);
    if (syntax != null)
      body = syntax.fromDatumIfNeeded(body);
    return body;
  }

  public Object skipAttrs(LambdaExp lexp, Object body, Translator tr)
  {
    while (body instanceof Pair)
      {
	Pair pair = (Pair) body;
	Object attr;
	if (! (pair.cdr instanceof Pair))
	  break;
	Object attrName = pair.car;
	Object attrValue = ((Pair) pair.cdr).car;
	if (tr.matches(attrName, "::"))
	  attrName = null;
	else if (! (attrName instanceof Keyword))
	  break;
	body = ((Pair) pair.cdr).cdr;
      }
    return body;
  }

  public void rewriteBody(LambdaExp lexp, Object body, Translator tr)
  {
    int numRenamedAlias = 0;
    tr.mustCompileHere();
    tr.pushScope(lexp);
    Declaration prev = null;
    int key_args = lexp.keywords == null ? 0 : lexp.keywords.length;
    int opt_args = lexp.defaultArgs == null ? 0
      : lexp.defaultArgs.length - key_args;
    int arg_i = 0;
    int opt_i = 0;
    for (Declaration cur = lexp.firstDecl(); cur != null; cur = cur.nextDecl())
      {
	if (cur.isAlias())
	  {
	    Declaration param = Translator.getOriginalRef(cur).getBinding();
	    lexp.replaceFollowing(prev, param);
	    param.context = lexp;
	    tr.pushRenamedAlias(cur);
	    numRenamedAlias++;
	    cur = param;
	  }
	prev = cur;

        if (arg_i >= lexp.min_args
            && (arg_i < lexp.min_args + opt_args
                || lexp.max_args >= 0
                || arg_i != lexp.min_args + opt_args))
          {
            lexp.defaultArgs[opt_i] = tr.rewrite(lexp.defaultArgs[opt_i]);
            opt_i++;
          }
        arg_i++;

        tr.lexical.push(cur);
      }

    if (lexp.isClassMethod()
        && ! lexp.nameDecl.getFlag(Declaration.STATIC_SPECIFIED))
      {
        // We set the type of this in ClassExp.walkChildren.
        lexp.add(null, new Declaration(ThisExp.THIS_NAME));
      }

    lexp.body = tr.rewrite_body (body);
    Type rtype;
    BeginExp bexp;
    Expression[] exps;
    int len;
    if (lexp.body instanceof BeginExp
        && (len = (exps = (bexp = (BeginExp) lexp.body).getExpressions()).length) > 1
        && exps[0] instanceof ReferenceExp)
      {
	// Handle '<TYPENAME> BODY':
        Expression rexp = exps[0];
        len--;
        if (len == 1)
          lexp.body = exps[1];
        else
          {
            Expression[] new_body = new Expression[len];
            System.arraycopy(exps, 1, new_body, 0, len);
            lexp.body = new BeginExp(new_body);
          }
        Convert.setCoercedReturnValue(lexp, rexp, tr.getLanguage());
      }
    else if (lexp.returnType != null
             && lexp.returnType != Type.pointer_type
             && lexp.returnType != Type.void_type)
      {
	Expression value = lexp.body;
	lexp.body = Convert.makeCoercion(value, lexp.returnType);
	lexp.body.setLine(value);
      }
    tr.pop(lexp);
    lexp.countDecls();
    tr.popRenamedAlias(numRenamedAlias);
    lexp.countDecls();
  }


  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}

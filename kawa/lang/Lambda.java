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
    rewrite(lexp, pair.car, pair.cdr, tr);
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
  public void rewrite(LambdaExp lexp, Object formals, Object body, Translator tr)
  {
    rewrite(lexp, formals, tr);
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());
    body = rewriteAttrs(lexp, body, tr);
    rewriteBody(lexp, body, tr);
  }

  public void rewrite(LambdaExp lexp, Object formals, Translator tr)
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    int opt_args = -1;
    int rest_args = -1;
    int key_args = -1;
    Pair pair;
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
        // An initial pass to count the parameters.
	if (pair.car == optionalKeyword)
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
	else if (pair.car == restKeyword)
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
	else if (pair.car == keyKeyword)
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
    for (; bindings instanceof Pair;  bindings = pair.cdr)
      {
	pair = (Pair) bindings;
	if (pair.car == optionalKeyword
	    || pair.car == restKeyword || pair.car == keyKeyword)
	  {
	    mode = pair.car;
	    continue;
	  }
	Object savePos = tr.pushPositionOf(pair);
	Object name;
	Object defaultValue = defaultDefault;
	Pair typeSpecPair = null;
        Pair p;
	if (tr.matches(pair.car, "::"))
	  {
	    tr.syntaxError("'::' must follow parameter name");
	    return;
	  }
	if (pair.car instanceof String || pair.car instanceof Symbol)
          {
            name = pair.car;
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
	else if (pair.car instanceof Pair
		 && ((p = (Pair) pair.car).car instanceof String
		     || p.car instanceof Symbol)
		 && p.cdr instanceof Pair)
          {
	    name = p.car;
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
	else
	  {
	    tr.syntaxError ("parameter is neither name nor (name :: type) nor (name default)");
	    return;
	  }
	if (mode == optionalKeyword || mode == keyKeyword)
	  lexp.defaultArgs[opt_args++] = new LangExp(defaultValue);
	if (mode == keyKeyword)
	  lexp.keywords[key_args++]
	    = Keyword.make(name instanceof Symbol ? ((Symbol) name).getName()
			   : name.toString());
	Declaration decl = lexp.addDeclaration (name);
        if (bindings instanceof PairWithPosition)
          {
            PairWithPosition declPos = (PairWithPosition) bindings;
            decl.setFile(declPos.getFile());
            decl.setLine(declPos.getLine(), declPos.getColumn());
          }
	if (typeSpecPair != null)
	  {
	    decl.setType(tr.exp2Type(typeSpecPair));
	    decl.setFlag(Declaration.TYPE_SPECIFIED);
	  }
	else if (mode == restKeyword)
	  decl.setType(Compilation.scmListType);
	decl.noteValue(null);  // Does not have a known value.
	tr.popPositionOf(savePos);
      }
    if (bindings instanceof String || bindings instanceof Symbol)
      {
	Declaration decl = lexp.addDeclaration (bindings);
	decl.setType(Compilation.scmListType);
	decl.noteValue (null);  // Does not have a known value.
      }
  }

  public Object rewriteAttrs(LambdaExp lexp, Object body, Translator tr)
  {
    String accessFlagName = null;
    String allocationFlagName = null;
    int accessFlag = 0;
    int allocationFlag = 0;
    while (body instanceof Pair)
      {
	Pair pair1 = (Pair) body;
	if (! (pair1.cdr instanceof Pair))
	  break;
	Object attrName = pair1.car;
	Pair pair2 = (Pair) pair1.cdr;

	Object attrValue = pair2.car;
	if (tr.matches(attrName, "::"))
	  attrName = null;
	else if (! (attrName instanceof Keyword))
	  break;
	if (attrName == null)
	  {
	    Expression attrExpr = tr.rewrite_car(pair2, false);
	    gnu.bytecode.Type rtype
	      = tr.getInterpreter().getTypeFor(attrExpr);
	    if (rtype != null)
	      lexp.setReturnType(rtype);
	  }
	else if (attrName == kawa.standard.object.accessKeyword)
	  {
	    Expression attrExpr = tr.rewrite_car(pair2, false);
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
	    Expression attrExpr = tr.rewrite_car(pair2, false);
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
	    int count = LList.listLength(attrValue, false);
	    if (count < 0)
	      tr.error('e', "throws: not followed by a list");
	    else
	      {
		ReferenceExp[] exps = new ReferenceExp[count];
		for (int i = 0;  i < count; i++)
		  {
		    pair2 = (Pair) attrValue;
		    Expression throwsExpr = tr.rewrite_car(pair2, false);
		    if (throwsExpr instanceof ReferenceExp)
		      {
			exps[i] = (ReferenceExp) throwsExpr;
		      }
		    else
		      {
			Object savePos = tr.pushPositionOf(pair2);
			tr.error('e', "throws not followed by a classname");
			tr.popPositionOf(savePos);
		      }
		    attrValue = pair2.cdr; 
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
    tr.push(lexp);
    if (lexp.defaultArgs != null)
      for (int i = 0, n = lexp.defaultArgs.length;  i < n;  i++)
	lexp.defaultArgs[i] = tr.rewrite(lexp.defaultArgs[i]);

    lexp.body = tr.rewrite_body (body);
    Type rtype;
    if (lexp.body instanceof BeginExp)
      {
	BeginExp bexp = (BeginExp) lexp.body;
	Expression[] exps = bexp.getExpressions();
	int len = exps.length;
	// Handle '<TYPENAME> BODY':
	if (len > 1 && exps[0] instanceof ReferenceExp)
	  {
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
	    Convert.setCoercedReturnValue(lexp, rexp, tr.getInterpreter());
	  }
      }
    else if (lexp.returnType != null && lexp.returnType != Type.pointer_type)
      {
	Expression value = lexp.body;
	lexp.body = Convert.makeCoercion(value, lexp.returnType);
	lexp.body.setLine(value);
      }
    tr.pop(lexp);
  }


  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}

package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.ClassType;
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
  // FIXME make method of Translator
  public void rewrite(LambdaExp lexp, Object formals, Object body, Translator tr)
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

    tr.push(lexp);
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
	String name;
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
            name = pair.car.toString();
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
	    name = p.car.toString();
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
	  lexp.defaultArgs[opt_args++] = tr.rewrite(defaultValue);
	if (mode == keyKeyword)
	  lexp.keywords[key_args++] = Keyword.make(name.toString());
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
	tr.push(decl);
      }
    if (bindings instanceof String || bindings instanceof Symbol)
      {
	Declaration decl = lexp.addDeclaration (bindings.toString());
	decl.setType(Compilation.scmListType);
	decl.noteValue (null);  // Does not have a known value.
	tr.push(decl);
      }
    if (body instanceof PairWithPosition)
      lexp.setFile(((PairWithPosition) body).getFile());
    rewriteBody(lexp, body, tr);
  }

  public void rewriteBody(LambdaExp lexp, Object body, Translator tr)
  {
    // Syntatic sugar:  <TYPE> BODY (or :: <TYPE> BODY) --> (as <TYPE> BODY)
    if (body instanceof Pair && tr.matches(((Pair) body).car, "::"))
      body = ((Pair) body).cdr;
    if (body instanceof Pair && ((Pair) body).car == "<sequence>")
      {
        ClassType type = ClassType.make("gnu.lists.Consumer");
        Declaration rdecl = new Declaration("$result$", type);
        lexp.add(null, rdecl);
        tr.push(rdecl);
        lexp.min_args++;
        if (lexp.max_args >= 0)
          lexp.max_args++;
        lexp.setFlag(true, LambdaExp.SEQUENCE_RESULT);
        body = ((Pair) body).cdr;
      }
    lexp.body = tr.rewrite_body (body);
    if (lexp.body instanceof BeginExp)
      {
	BeginExp bexp = (BeginExp) lexp.body;
	Expression[] exps = bexp.getExpressions();
	int len = exps.length;
	if (len > 1)
	  {
	    Expression rexp = exps[0];
	    gnu.bytecode.Type rtype = tr.getInterpreter().getTypeFor(rexp);
	    if (rtype != null)
	      {
                len--;
                Expression value;
                if (len == 1)
                  value = exps[1];
                else
                  {
                    Expression[] new_body = new Expression[len];
                    System.arraycopy(exps, 1, new_body, 0, len);
                    value = new BeginExp(new_body);
                  }
                lexp.body = Convert.makeCoercion(value, rexp);
		lexp.setReturnType(rtype);
	      }
	  }
      }
    tr.pop(lexp);
  }


  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<builtin lambda>");
  }
}

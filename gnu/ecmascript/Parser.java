package gnu.ecmascript;
import kawa.lang.*;


public class Parser
{
  InPort port;
  Lexer lexer;

  Object previous_token;
  Object token;

  public static Expression eofExpr = new QuoteExp(Sequence.eofValue);

  public Parser (InPort port)
  {
    this.port = port;
    this.lexer = new Lexer(port);
  }

  public Expression parseLogicalORExpression()
    throws java.io.IOException, kawa.lang.ReadError
  {
    return parseBinaryExpression(4);
  }

  public Expression parseConditionalExpression()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Expression exp1 = parseLogicalORExpression();
    Object result = peekToken();
    if (result != Lexer.condToken)
      return exp1;
    skipToken();
    Expression exp2 = parseAssignmentExpression();
    if (getToken() != Lexer.colonToken)
      throw new ReadError(port, "expected ':' in conditional expression");
    Expression exp3 = parseAssignmentExpression();
    return new IfExp(exp1, exp2, exp3);
  }

  public Expression parseAssignmentExpression()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Expression exp1 = parseConditionalExpression();
    Object token = peekToken();
    if (token == Lexer.equalToken)
      {
	skipToken();
	Expression exp2 = parseAssignmentExpression();
	if (exp1 instanceof ReferenceExp)
	  {
	    SetExp sex = new SetExp(((ReferenceExp) exp1).getName(), exp2);
	    sex.setDefining(true);
	    return sex;
	  }
	throw new ReadError(port, "unmplemented non-symbol ihs in assignment");
      }
    else
      {
	if (! (token instanceof Reserved))
	  return exp1;
	Reserved op = (Reserved) token;
	if (!op.isAssignmentOp())
	  return exp1;
	skipToken();
	Expression exp2 = parseAssignmentExpression();
	Expression[] args = { exp1, exp2 };
	return new ApplyExp(new QuoteExp(op.proc), args);
      }
  }

  public Expression parseExpression()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Expression[] exps = null;
    int nExps = 0;
    for (;;)
      {
	Expression exp1 = parseAssignmentExpression();
	boolean last = peekToken() != Lexer.commaToken;
	if (exps == null)
	  {
	    if (last)
	      return exp1;
	    exps = new Expression[2];
	  }
	else if (last ? exps.length != nExps + 1 : exps.length <= nExps)
	  { // need to resize
	    int newsize = last ? nExps + 1 : 2 * exps.length;
	    Expression[] new_exps = new Expression[newsize];
	    System.arraycopy(exps, 0, new_exps, 0, nExps);
	    exps = new_exps;
	  }
	exps[nExps++] = exp1;
	if (last)
	  return new BeginExp(exps);
	skipToken();
      }
  }

  public Object peekToken()
    throws java.io.IOException, kawa.lang.ReadError
  {
    while (token == null)
      {
	token = lexer.getToken();
	OutPort out = OutPort.outDefault();
	out.print("token:");
	SFormat.print(token, out);
	out.println(" [class:"+token.getClass()+"]");
	if (token == Lexer.eolToken)
	  skipToken();
      }
    return token;
  }

  public Object getToken()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Object result = peekToken();
    skipToken();
    return result;
  }

  public final void skipToken()
  {
    if (token != Lexer.eofToken)
      {
	previous_token = token;
	token = null;
      }
  }

  /** Skip an explicit or implicit semicolon. */
  public void getSemicolon()
    throws java.io.IOException, kawa.lang.ReadError
  {
    token = peekToken();
    if (token == Lexer.semicolonToken)
      skipToken();
    else if (token == Lexer.rbraceToken
	     || token == Lexer.eofToken
	     || previous_token == Lexer.eolToken)
	; // implicit ("inserted") semicolon
    else
      throw new ReadError(port, "missing ';' after expression");
  }


  public Expression parsePrimaryExpression()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Object result = getToken();
    if (result instanceof QuoteExp)
      return (QuoteExp) result;
    if (result instanceof String)
      return new ReferenceExp((String) result);
    if (result == Lexer.lparenToken)
      {
	Expression expr = parseExpression();
	Object token = getToken();
	if (token != Lexer.rparenToken)
	  return syntaxError("expected ')' - got:"+token);
	return expr;
      }
    return syntaxError("unexpected token: "+result);
  }

  public Expression parseUnaryExpression ()
    throws java.io.IOException, kawa.lang.ReadError
  {
    // FIXME
    return parsePrimaryExpression();
  }

  public int errors;

  public Expression syntaxError(String message)
  {
    // same as Translator.syntaxError.  FIXME
    errors++;
    OutPort err = OutPort.errDefault();
    String current_filename = port.getName();
    int current_line = port.getLineNumber()+1;
    int current_column = port.getColumnNumber()+1;
    if (current_line > 0)
      {
	if (current_filename != null)
	  err.print (current_filename);
	err.print (':');
	err.print (current_line);
	if (current_column > 1)
	  {
	    err.print (':');
	    err.print (current_column);
	  }
	err.print (": ");
      }
    err.println (message);
    return new ErrorExp (message);
  }

  /*
  public Expression parseBinaryExpression(int prio)
    throws java.io.IOException, kawa.lang.ReadError
  {
    if (prio > 10)
      return parseUnaryExpression();
    Expression exp1 = parseBinaryExpression(prio+2);
    token = peekToken();
    for(;;)
      {
	if (! (token instanceof Reserved))
	  break;
	Reserved op = (Reserved) token;
	if (op.prio != prio)
	  break;
	getToken();
	Expression exp2 = parseBinaryExpression(prio+2);
	Expression[] args = { exp1, exp2 };
	ApplyExp exp = new ApplyExp(new QuoteExp(op.proc), args);
	exp1 = exp;
      }
    return exp1;
  }
  */

  public Expression parseBinaryExpression(int prio)
    throws java.io.IOException, kawa.lang.ReadError
  {
    Expression exp1 = parseUnaryExpression();
    for (;;)
      {
	token = peekToken();
	if (! (token instanceof Reserved))
	  return exp1;
	Reserved op = (Reserved) token;
	if (op.prio < prio)
	  return exp1;
	getToken();
	Expression exp2 = parseBinaryExpression(op.prio+2);
	Expression[] args = { exp1, exp2 };
	exp1 = new ApplyExp(new QuoteExp(op.proc), args);
      }
  }

  static Expression emptyStatement = new QuoteExp(Interpreter.voidObject);

  public Expression parseStatement()
    throws java.io.IOException, kawa.lang.ReadError
  {
    Object token = peekToken();
    if (token == Lexer.eofToken)
      return eofExpr;
    if (token == Lexer.semicolonToken)
      {
	skipToken();
	return emptyStatement;
      }
    if (token == Lexer.lbraceToken)
      {
	Expression[] exps = null;
	skipToken();
	int nExps = 0;
	for (;;)
	  {
	    token = peekToken();
	    boolean last;
	    if (token == Lexer.rbraceToken)
	      {
		skipToken();
		if (exps == null)
		  return emptyStatement;
		last = true;
	      }
	    else
	      last = false;
	    if (exps == null)
	      exps = new Expression[2];
	    else if (last ? exps.length !=nExps : exps.length <= nExps) 
	      { // need to resize 
		int newsize = last ? nExps : 2 * exps.length; 
		Expression[] new_exps = new Expression[newsize]; 
		System.arraycopy(exps, 0, new_exps, 0, nExps); 
		exps = new_exps; 
	      } 
	    if (last)
	      return new BeginExp(exps);
	    exps[nExps++] = parseStatement();
	  }
      }
    
    Expression exp = parseExpression();
    getSemicolon();
    return exp;
  }

  public static void main (String[] args)
  {
    Interpreter interp = new kawa.standard.Scheme();  // FIXME
    Environment.setCurrent(interp.getEnvironment());

    Parser parser = new Parser(InPort.inDefault());
    OutPort out = OutPort.outDefault();
    for (;;)
      {
	try
	  {
	    /*
	    Object token = parser.peekToken();
	    if (token == Lexer.eofToken)
	      break;
	    if (token == Lexer.eolToken)
	      {
		parser.getToken();
		continue;
	      }
	    Expression expr = parser.parseExpression();
	    */
	    Expression expr = parser.parseStatement();
	    if (expr == eofExpr)
	      break;
	    out.print("[Expression: ");
	    expr.print(out);
	    out.println("]");
	    Object result = expr.eval(Environment.user());
	    out.print("result: ");
	    SFormat.print(result, out);
	    out.println();
	  }
	catch (Exception ex)
	  {
	    System.err.println("caught exception:"+ex);
	    ex.printStackTrace(System.err);
	    return;
	  }
      }
  }
}

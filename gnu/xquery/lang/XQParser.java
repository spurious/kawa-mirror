// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.text.*;
import gnu.expr.*;
import java.util.Vector;

/** A class to read xquery forms. */

public class XQParser extends LispReader // should be extends Lexer
{
  int curToken;
  Object curValue;

  public static final gnu.kawa.reflect.InstanceOf instanceOf
  = new gnu.kawa.reflect.InstanceOf(XQuery.getInstance(), "instance");

  int nesting;

  /** Skip whitspace.
   * Sets 'index' to the that of the next non-whitespace character,
   * and returns that.  If there are no more non-space characters,
   * returns ' '.  */
  final int skipSpace()
    throws java.io.IOException
  {
    for (;;)
      {
	int ch = read();
	if (ch < 0 || ! Character.isWhitespace((char) ch))
	  return ch;
      }
  }

  final int skipHSpace()
    throws java.io.IOException
  {
    for (;;)
      {
	int ch = read();
	if (ch != ' ' && ch != '\t')
	  return ch;
      }
  }

  static final int EOF_TOKEN = -1;
  static final int EOL_TOKEN = '\n';
  static final char INTEGER_TOKEN = '0';
  static final char FLOAT_TOKEN = '1';
  static final int STRING_TOKEN = '"';
  static final int SLASHSLASH_TOKEN = 'D';
  static final int DOTDOT_TOKEN = '2';
  static final int COLONCOLON_TOKEN = 'X';

  /** A non-qualified (simple) name (NCName).
   * The tokenBuffer contains the name (which does not contain a ':'). */
  static final int NCNAME_TOKEN = 'A';

  /** A Qualified name (QName).
   * The tokenBuffer contains the full name, whihc contains one ':'. */
  static final int QNAME_TOKEN = 'Q';

  static final int ARROW_TOKEN = 'R';

  /* FuncName including following '('). */
  static final int FNAME_TOKEN = 'F';

  /* 'Q': QName (intern'ed name is curValue)
   * 'R': NCName ':' '*'
   * OP_AXIS_FIRST: 'ancestor' followed by '::'
   * ...
   * OP_AXIS_FIRST+AXIS_SELF: 'self' followed by '::'
   */

  static final int OP_AXIS_FIRST = 100;
  static final int COUNT_OP_AXIS = 13;
  static final int AXIS_ANCESTOR = 0;
  static final int AXIS_ANCESTOR_OR_SELF = 1;
  static final int AXIS_ATTRIBUTE = 2;
  static final int AXIS_CHILD = 3;
  static final int AXIS_DESCENDANT = 4;
  static final int AXIS_DESCENDANT_OR_SELF = 5;
  static final int AXIS_FOLLOWING = 6;
  static final int AXIS_FOLLOWING_SIBLING = 7;
  static final int AXIS_NAMESPACE = 8;
  static final int AXIS_PARENT = 9;
  static final int AXIS_PRECEDING = 10;
  static final int AXIS_PRECEDING_SIBLING = 11;
  static final int AXIS_SELF = 12;
  // Token types for binary operators.
  // When used as a token code, get the priority by shifting 2 right.
  static final int OP_OR        = 200;          // 'or'
  static final int OP_AND       = 200 + 4;      // 'and'
  static final int OP_EQU       = 200 + 8;      // '='
  static final int OP_NEQ       = 200 + 8 + 1;  // '!='
  static final int OP_INSTANCEOF= 200 + 8 + 2;  // 'instanceof'
  static final int OP_RANGE_TO  = 200 + 8 + 3;  // 'to'
  static final int OP_LSS       = 200 + 12;     // '<'
  static final int OP_GRT       = 200 + 12 + 1; // '>'
  static final int OP_LEQ       = 200 + 12 + 2; // '<='
  static final int OP_GEQ       = 200 + 12 + 3; // '>='
  static final int OP_ADD       = 200 + 16;     // '+'
  static final int OP_SUB       = 200 + 16 + 1; // '-'
  static final int OP_MUL       = 200 + 20;     // '*'
  static final int OP_DIV       = 200 + 20 + 1; // 'div'
  static final int OP_MOD       = 200 + 20 + 2; // 'mod'
  static final int OP_INTERSECT = 200 + 24;     // 'intersect'
  static final int OP_EXCEPT    = 200 + 24 + 1; // 'except'
  static final int OP_UNION     = 200 + 24 + 2; // 'except'
  
  /**
   * An encoding of the token type:
   * '\uffff': end of string.
   * '(', ')', '[', ']', '.', '@', ',', '$': ` That character token.
   */

  boolean checkNext(char ch)
      throws java.io.IOException
  {
    if (peek() != ch)
      return false;
    skip();
    return true;
  }

  public static boolean isNameStart(char ch)
  {
    return Character.isLetter(ch) || ch == '_';
  }

  public static boolean isNamePart(char ch)
  {
    return Character.isUnicodeIdentifierPart(ch) || ch == '-' || ch == '.';
  }

  int getRawToken()
      throws java.io.IOException, SyntaxException
  {
    int next;
    for (;;)
      {
	next = read();
	if (next < 0)
	  return curToken = EOF_TOKEN;
	if (next == '\n' || next == '\r')
	  {
	    if (nesting > 0)
	      continue;
	    return curToken = EOL_TOKEN;
	  }
	if (next != ' ' && next != '\t')
	  break;
      }
    tokenBufferLength = 0;
    char ch = (char) next;
    switch (ch)
      {
      case '(':  case ')':  case '[':  case ']':  case '{':  case '}':
      case '$':  case '@':  case ',':  case ':':
	break;
      case '|':
	ch = OP_UNION;
	break;
      case '*':
	ch = OP_MUL;
	break;
      case '+':
	ch = OP_ADD;
	break;
      case '-':
	ch = OP_SUB;
	break;
      case '!':
	if (checkNext('='))
	  ch = OP_NEQ;
	break;
      case '/':
	if (checkNext('/'))
	  ch = SLASHSLASH_TOKEN;
	break;
      case '=':
	if (checkNext('>'))
	  ch = ARROW_TOKEN;
	ch = OP_EQU;
	break;
      case '>':
	ch = checkNext('=') ? (char) OP_GEQ : (char) OP_GRT;
	break;
      case '<':
	ch = checkNext('=') ? (char) OP_LEQ : (char) OP_LSS;
	break;
      case '.':
	if (checkNext('.'))
	  {
	    ch = DOTDOT_TOKEN;
	    break;
	  }
	next = peek();
	if (Character.isDigit((char) next))
	  {
	    tokenBufferAppend('.');
	    for (;;)
	      {
		tokenBufferAppend((char) next);
		skip();
		next = peek();
		if (! Character.isDigit((char) next))
		  break;
	      }
	    ch = FLOAT_TOKEN;
	  }
	break;
      case '\'':  case '\"':
	for (;;)
	  {
	    next = read();
	    if (next < 0)
	      eofError("unexpected end-of-file in string");
	    if (ch == next)
	      break;
	    tokenBufferAppend((char) next);
	  }
	ch = STRING_TOKEN;
	break;
      default:
	if (Character.isDigit(ch))
	  {
	    boolean seenDot = false;
	    for (;; )
	      {
		tokenBufferAppend(ch);
		next = read();
		if (next < 0)
		  break;
		ch = (char) next;
		if (ch == '.')
		  {
		    if (seenDot)  break;
		    seenDot = true;
		  }
		else if (! Character.isDigit(ch))
		  break;
	      }
	    if (next == 'e' || next == 'E')
	      {
		next = read();
		if (next == '+' || next == '-')
		  {
		    tokenBufferAppend((char) next);
		    next = read();
		  }
		int expDigits = 0;
		for (;;)
		  {
		    if (next < 0)
		      break;
		    ch = (char) next;
		    if (! Character.isDigit(ch))
		      {
			unread();
			break;
		      }
		    tokenBufferAppend(ch);
		    next = read();
		    expDigits++;
		  }
		if (expDigits == 0)
		  error("no digits following exponent");
		ch = FLOAT_TOKEN;
	      }
	    else
	      {
		ch = seenDot ? FLOAT_TOKEN : INTEGER_TOKEN;
		if (next >= 0)
		  unread(next);
	      }
	  }
	else if (isNameStart(ch))
	  {
	    for (;;)
	      {
		tokenBufferAppend(ch);
		next = read();
		ch = (char) next;
		if (! isNamePart(ch))
		  break;
	      }
	    if (next < 0)
	      ch = NCNAME_TOKEN;
	    else if (next == ':')
	      {
		next = read();
		if (next < 0)
		  eofError("unexpected end-of-file after NAME ':'");
		ch = (char) next;
		if (isNameStart(ch))
		  {
		    tokenBufferAppend(':');
		    for (;;)
		      {
			tokenBufferAppend(ch);
			next = read();
			ch = (char) next;
			if (! isNamePart(ch))
			  break;
		      }
		    ch = QNAME_TOKEN;
		  }
		else
		  {
		    // This is the only place where we need an extra
		    // character look-ahead, and may need to back up twice.
		    // Luckily, that is safe since we use a LineBufferedReader.
		    // An alterative would be to add special tokens for
		    // NCName+"::" and NCName+":=".  That would complicate
		    // the parser a bit.
		    unread(next);
		    unread(':');
		    ch = NCNAME_TOKEN;
		  }
	      }
	    else
	      {
		unread();
		ch = NCNAME_TOKEN;
	      }
	  }
	else if (ch >= ' ' && ch < 127)
	  error("invalid character '"+ch+'\'');
	else
	  error("invalid character '\\u"+Integer.toHexString(ch)+'\'');
      }
    curToken = ch;
    return ch;
  }

  /** Return the current token, assuming it is in operator context.
   * Resolve NCNAME_TOKEN (identifier) to 'and', 'or', 'div', etc.
   */
  int peekOperator()
      throws java.io.IOException, SyntaxException
  {
    while (curToken == EOL_TOKEN)
      getRawToken();
    if (curToken == NCNAME_TOKEN)
      {
	int len = tokenBufferLength;
	if (len == 2 || len == 3)
	  {
	    char c1 = tokenBuffer[0];
	    if (len == 2)
	      {
		if (c1 == 'o' && tokenBuffer[1] == 'r')
		  curToken = OP_OR;
		else if (c1 == 't' && tokenBuffer[1] == 'o')
		  curToken = OP_RANGE_TO;
	      }
	    else
	      {
		char c2 = tokenBuffer[1];
		char c3 = tokenBuffer[2];
		if (c1 == 'a')
		  {
		    if (c2 == 'n' && c3 == 'd')
		      curToken = OP_AND;
		  }
		else if (c1 == 'm') {
		  if (c2 == 'u' && c3 == 'l')
		    curToken = OP_MUL;
		  if (c2 == 'o' && c3 == 'd')
		    curToken = OP_MOD;
		}
		else if (c1 == 'd') {
		  if (c2 == 'i' && c3 == 'v')
		    curToken = OP_DIV;
		}
	      }
	  }
	else if (len == 10)
	  {
	    for (int i = 0; ;   i++)
	      {
		if (i == 10)
		  {
		    curToken = OP_INSTANCEOF;
		    break;
		  }
		else if (tokenBuffer[i] != "instanceof".charAt(i))
		  break;
	      }
	  }
      }
    return curToken;
  }

  /** Process token, assuming we are in operand context.
   */

  int peekOperand()
      throws java.io.IOException, SyntaxException
  {
    while (curToken == EOL_TOKEN)
      getRawToken();
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next == '('
	    && ! name.equalsIgnoreCase("if"))
	  // FunctionCall or KindTest
	  return FNAME_TOKEN;
	//int next = peek();
	if (next != ':')
	  {
	    unread();
	    return curToken;
	  }
	next = read();
	if (next == ':')
	  {
	    next = read();
	    if (next == ':') // We've seen an Axix specifier.
	      {
		// match axis name
		name = name.intern();
		int i;
		for (i = COUNT_OP_AXIS;  --i >= 0; )
		  if (axisNames[i] == name)
		    break;
		if (i >= 0)
		  curToken = (char) (OP_AXIS_FIRST + i);
		else
		  error("unknown axis name '" + name + '\'');
		curValue = name;
		return curToken;
	      }
	    /*
	    else if (next == '*')
	      {
		// FIXME
		curToken = 'R';
	      }
	    */
	    else
	      error("invalid character after ':'");
	  }
	else
	  {
	    curToken = QNAME_TOKEN;
	    curValue = name;
	  }
	/*
	if (curToken == QNAME_TOKEN && next == '(')
	  {
	    name = name.intern();
	    int i;
	    for (i = 4;  --i >= 0; )
	      {
		if (nodeTypeNames[i] == name)
		  break;
	      }
	    index++;
	    if (i >= 0)
	      curToken = (char) (OP_NODE + i);
	    else
	      curToken = 'F';  // FuncName.
	  }
	*/
      }
    return curToken;
  }


 public XQParser (InPort port)
  {
    super(port);
    nesting = port instanceof TtyInPort ? 0 : 1;
  }

  public XQParser(InPort port, SourceMessages messages)
  {
    super(port, messages);
    nesting = port instanceof TtyInPort ? 0 : 1;
  }
  
  protected ReadTable getReadTable () { return xqlReadTable; }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new XQParser(port)).readObject();
  }

  public static ReadTable xqlReadTable;
  static
  {
    xqlReadTable = ReadTable.getInitial();
  }

  private static final int priority(int opcode) { return opcode >> 2; }

  int count = 0;

  Expression parseExpr()
      throws java.io.IOException, SyntaxException
  {
    return parseSortExpr();
  }

  static Expression makeBinary(Expression func,
			       Expression exp1, Expression exp2)
  {
    Expression[] args = new Expression[2];
    args[0] = exp1;
    args[1] = exp2;
    return new ApplyExp(func, args);
  }

  static Expression makeExprSequence(Expression exp1, Expression exp2)
  {
    return makeBinary(makeFunctionExp
		      ("gnu.xquery.util.AppendValues", "appendValues"),
		      exp1, exp2);
  }

  Expression makeBinary(int op, Expression exp1, Expression exp2)
  {
    Expression func;
    switch (op)
      {
      case OP_ADD: 
	func = makeFunctionExp("gnu.kawa.functions.AddOp", "+");
	break;
      case OP_SUB:
	func = makeFunctionExp("gnu.kawa.functions.AddOp", "-");
	break;
      case OP_MUL:
	func = makeFunctionExp("gnu.kawa.functions.MultiplyOp", "$St", "mul");
	break;
      case OP_DIV:
	func = makeFunctionExp("gnu.kawa.functions.DivideOp", "$Sl", "div");
	break;
      case OP_MOD:
	func = new QuoteExp(new PrimProcedure(gnu.bytecode.ClassType.make("gnu.math.IntNum").getDeclaredMethod("remainder", 2)));
	break;
      case OP_LSS:
	func = makeFunctionExp("gnu.kawa.functions.NumberCompare", "<");
	break;
      case OP_LEQ:
	func = makeFunctionExp("gnu.kawa.functions.NumberCompare", "<=");
	break;
      case OP_GRT:
	func = makeFunctionExp("gnu.kawa.functions.NumberCompare", ">");
	break;
      case OP_GEQ:
	func = makeFunctionExp("gnu.kawa.functions.NumberCompare", ">=");
	break;
      case OP_RANGE_TO:
	func = makeFunctionExp("gnu.xquery.util.IntegerRange", "integerRange");
	break;
      default:
	return new ErrorExp("unimplemented binary op: "+op);
      }
    return makeBinary(func, exp1, exp2);
  }

  Expression parseSortExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseBinaryExpr(priority(OP_OR));
    // FIXME check for "sortby".
    return exp;
  }

  public Expression parseDataType()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String tname = new String(tokenBuffer, 0, tokenBufferLength);
	gnu.bytecode.Type type = kawa.standard.Scheme.getNamedType(tname);
	getRawToken();
	return new QuoteExp(type);
      }
    else
      {
	getRawToken();
	return syntaxError("bad syntax - expected DataType");
      }
  }

  Expression parseBinaryExpr(int prio)
      throws java.io.IOException, SyntaxException
  {
    Expression exp;
    exp = parseUnaryExpr();
    for (;;)
      {
	if (nesting == 0 && curToken == EOL_TOKEN)
	  return exp;
	int token = peekOperator();
	if (token == OP_LSS && peek() == '/')
	  return exp;  // Makes for better error handling.
	int tokPriority = priority(token);
	if (tokPriority < prio || tokPriority > (OP_MOD >> 2))
	  return exp;
	getRawToken();
	if (token == OP_INSTANCEOF)
	  {
	    Expression[] args = { exp, parseDataType() };
	    exp = new ApplyExp(makeFunctionExp("gnu.xquery.lang.XQParser",
					       "instanceOf"),
			       args);
	  }
	else
	  {
	    Expression exp2 = parseBinaryExpr(tokPriority+1);
	    if (token == OP_AND)
	      exp = new IfExp(exp, exp2, QuoteExp.falseExp);
	    else
	      exp = makeBinary(token, exp, exp2);
	  }
    }
  }

  Expression parseUnaryExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp;
    if (curToken == OP_SUB || curToken == OP_ADD) {
      getRawToken();
      exp = parseUnionExpr();
      exp = syntaxError("non-trivial UnaryExpr not implemented");
    }
    else
      exp = parseUnionExpr();
    return exp;
  }

  Expression parseUnionExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseIntersectExceptExpr();
    while (curToken == OP_UNION)
      {
	int op = curToken;
	getRawToken();
	Expression exp2 = parseIntersectExceptExpr();
	exp = makeBinary(op, exp, exp2);
      }
    return exp;
  }

  Expression parseIntersectExceptExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parsePathExpr();
    while (curToken == OP_INTERSECT || curToken == OP_EXCEPT)
      {
	int op = curToken;
	getRawToken();
	Expression exp2 = parsePathExpr();
	exp = makeBinary(op, exp, exp2);
      }
    return exp;
  }

  Expression parsePathExpr()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      return syntaxError("unimplemented non-relative PathExpr");
    return parseRelativePathExpr();
  }

  Expression parseRelativePathExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseStepExpr();
    while (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	int op = curToken;
	getRawToken();
	// Kludge for special case.
	if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
	  {
	    String name = new String(tokenBuffer, 0, tokenBufferLength);;
	    Expression[] args = { exp, new QuoteExp(name.intern()) };
	    exp = new ApplyExp(makeFunctionExp("gnu.xquery.util.NamedChildren", "namedChildren"),
			       args);
	    getRawToken();
	  }
	else
	  exp = makeBinary(op, exp, parseStepExpr());
      }
    return exp;
  }

  Expression parseStepExpr()
      throws java.io.IOException, SyntaxException
  {
    if (curToken >= OP_AXIS_FIRST
	&& curToken < OP_AXIS_FIRST + COUNT_OP_AXIS)
      {
	// return parseAxisStepExpr();
	return syntaxError("parseAxiStepExpr not implemented");
      }
    return parseOtherStepExpr();
  }

  /* Parse an OtherStepExpr.
   */
  Expression parseOtherStepExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parsePrimaryExpr();
    for (;;)
      {
	/*
	if (curToken == '[')
	  ...;
	else if (curToken == ARROW_TOKEN)
	  ...;
	else
	*/
	  return exp;
      }
  }

  /**
   * Parse a PrimaryExpr.
   * @return an Expression.
   */
  Expression parsePrimaryExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseMaybePrimaryExpr();
    if (exp == null)
      return syntaxError("missing PrimaryExpr");
    return exp;
  }

  /** Parse ElementContent (delimiter == '<')  or AttributeContext (otherwise).
   * @param delimiter is '<' if parsing ElementContent, is either '\'' or
   *   '\"' if parsing AttributeContent depening on the starting quote
   * @param result a buffer to place the resulting Expressions.
   */
  void parseContent(int delimiter, Vector result)
      throws java.io.IOException, SyntaxException
  {
    StringBuffer sbuf = new StringBuffer();
    for (;;)
      {
	int next = read();
	if ((next < 0 || next == '{' || next == delimiter)
	    && sbuf.length() > 0)
	  {
	    result.add(new QuoteExp(new FString(sbuf)));
	    sbuf.setLength(0);
	  }
	if (next < 0)
	  eofError("unexpected end-of-file");
	if (next == '{')
	  {
	    result.addElement(parseEnclosedExpr());
	  }
	else if (next == '<' && delimiter == '<')
	  {
	    next = read();
	    if (next == '/')
	      {
		break;
	      }
	    if (next >= 0)
	      unread(next);
	    getRawToken();
	    result.addElement(parseElementConstructor());
	  }
	else if (next == delimiter)
	  break;
	// FIXME handle char references etc.
	else
	  sbuf.append((char) next);
      }
  }

  /** Parse an EnclosedExpr.
   * Assume the '{' has been read.
   */
  Expression parseEnclosedExpr()
      throws java.io.IOException, SyntaxException
  {
    getRawToken();
    nesting++;
    Expression exp = parseExpr();
    nesting--;
    //do { getRawToken(); } while (curToken == EOL_TOKEN);
    while (curToken == ',')
      {
	getRawToken();
	exp = makeExprSequence(exp, parseExpr());
      }
    if (curToken != '}')
      return syntaxError("missing '}'");
    return exp;
  }

  Expression parseNameSpec()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	return new QuoteExp(name);  // FIXME
      }
    else if (curToken == '{')
      {
	return parseEnclosedExpr();
      }
    else
      return null;
  }

  /** Parse ElementConstructor.
   * Assume initial '<' has been processed.
   * Reads through end of the end tag.  FIXME
   */
  Expression parseElementConstructor()
      throws java.io.IOException, SyntaxException
  {
    nesting++;
    Vector vec = new Vector();
    Expression element = parseNameSpec();
    vec.addElement(element);
    if (element == null)
      return syntaxError("missing NameSpec");
    Expression[] args;
    int ch;
    for (;;)
      {
	ch = skipSpace();
	if (ch < 0 || ch == '>')
	  break;
	unread(ch);
	getRawToken();
	int vecSize = vec.size();
	Expression attrName = parseNameSpec();
	if (attrName == null)
	  break;
	vec.addElement(attrName);
	ch = skipSpace();
	if (ch != '=')
	  return syntaxError("missing '=' after attribute");
	ch = skipSpace();
	parseContent(ch, vec);
	args = new Expression[vec.size() - vecSize];
	for (int i = args.length;  --i>= 0; )
	  args[i] = (Expression) vec.elementAt(vecSize + i);
	vec.setSize(vecSize);
	vec.addElement(new ApplyExp(makeFunctionExp("gnu.xquery.util.MakeAttribute", "makeAttribute"),
				    args));
      }
    if (ch != '>')
      return syntaxError("missing '>' after start element");
    parseContent('<', vec);
    getRawToken();
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String tag = new String(tokenBuffer, 0, tokenBufferLength);
	// check that it `matches start tag FIXME
	getRawToken();
      }
    if (curToken != OP_GRT) // FIXME what about OP_GRE
      return syntaxError("missing '>' after end element");
    args = new Expression[vec.size()];
    vec.copyInto(args);
    nesting--;
    return new ApplyExp(makeFunctionExp("gnu.xquery.util.MakeElement", "makeElement"),
			args);
  }

  /**
   * Try to parse a PrimaryExpr.
   * @return an Expression, or null if no PrimaryExpr was seen.
   */
  Expression parseMaybePrimaryExpr()
      throws java.io.IOException, SyntaxException
  {
    int token = peekOperand();
    Expression exp;
    int c1, c2, c3;
    if (token == '(')
      {
	getRawToken();
	if (curToken == ')')
	  exp = QuoteExp.voidExp;
	else
	  {
	    exp = null;
	    for (;;)
	      {
		nesting++;
		Expression exp1 = parseExpr();
		exp = exp == null ? exp1 : makeExprSequence(exp, exp1);
		nesting--;
		if (curToken == ')')
		  break;
		if (curToken == EOF_TOKEN)
		  eofError("missing ')' - unexpected end-of-file");
		if (curToken != ',')
		  error("missing ')'");
		getRawToken();
	      }
	  }
      }
    else if (token == OP_LSS)
      {
	getRawToken();
	if (curToken == '/')
	  {
	    getRawToken();
	    String msg;
	    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
	      msg = "saw end tag '</" + new String(tokenBuffer, 0, tokenBufferLength) + ">' not in an element constructor";
	    else
	      msg = "saw end tag '</' not in an element constructor";
	    exp = syntaxError(msg);
	    while (curToken != OP_GRT && curToken != EOF_TOKEN && curToken != EOL_TOKEN)
	      getRawToken();
	    return exp;
	  }
	exp = parseElementConstructor();
      }
    else if (token == STRING_TOKEN)
      {
	//Object val = new String(tokenBuffer, 0, tokenBufferLength);
	Object val = new FString(tokenBuffer, 0, tokenBufferLength);
	exp = new QuoteExp(val);
      }
    else if (token == INTEGER_TOKEN)
      {
	Object val = gnu.math.IntNum.valueOf(tokenBuffer, 0, tokenBufferLength,
					     10, false);
	exp = new QuoteExp(val);
      }
    else if (token == FLOAT_TOKEN)
      {
	Object val = new gnu.math.DFloNum(new String(tokenBuffer, 0, tokenBufferLength));
	exp = new QuoteExp(val);
      }
    /*
    else if (token == '<')
      {
	// ElementConstructor
      }
    */
    else if (token == '$')
      {
	int next = peek();
	if (next < 0 || Character.isWhitespace((char) next))
	  return syntaxError("missing name after variable-name operator '$'");
	getRawToken();
	/*
	token = peekOperand();
	if (start != tokenStart || token != QNAME_TOKEN)
	  emit(curValue);
	exp = new QuoteExp(???);
	*/
	String name = new String(tokenBuffer, 0, tokenBufferLength).intern();
	Declaration decl = parser.lookup(name, -1);
	exp = new ReferenceExp(name, decl);
      }
    else if (token == FNAME_TOKEN)
      {
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	name = name.intern();
	nesting++;
	getRawToken();
	Vector vec = new Vector(10);
	if (curToken != ')')
	  {
	    for (;;)
	      {
		Expression arg = parseExpr();
		vec.addElement(arg);
		if (curToken == ')')
		  break;
		if (curToken != ',')
		  return syntaxError("missing ')' after function call");
		getRawToken();
	      }
	  }
	Expression[] args = new Expression[vec.size()];
	vec.copyInto(args);
	exp = new ApplyExp(new ReferenceExp(name), args);
	nesting--;
      }
    else if (token == NCNAME_TOKEN || token == QNAME_TOKEN)
      {
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next == '$')
	  {
	    // A FLWR-expression isn't technically a PrimaryExpr.
	    // Does it matter?  FIXME.
	    int forOrLet = -1;
	    if (tokenBufferLength == 3)
	      {
		// FIXME:  use match(String)?
		c1 = tokenBuffer[0];
		c2 = tokenBuffer[2];
		c3 = tokenBuffer[2];
		if ((c1 == 'l' || c1 == 'L')
		    || (c2 == 'e' || c1 == 'E')
		    || (c3 == 't' || c1 == 'T'))
		  forOrLet = 0;
		else if ((c1 == 'f' || c1 == 'F')
		    || (c2 == 'o' || c1 == 'O')
		    || (c3 == 'r' || c1 == 'R'))
		  forOrLet = 1;
	      }
	    if (forOrLet < 0)
	      return syntaxError("invalid syntax - variable following name");
	    return parseFLWRExpression(forOrLet > 0);
	  }
	else if (next == '(' && tokenBufferLength == 2
		 && ((c1 = tokenBuffer[0]) == 'i' || c1 == 'I')
		 && ((c2 = tokenBuffer[1]) == 'f' || c2 == 'F'))
	  {
	    return parseIfExpr();
	  }
	else
	  {
	    if (next >= 0)
	      unread();
	    new Error("NameTest").printStackTrace();
	    return syntaxError("NameTest not implememented");
	  }
      }
    else
      return null;
    /*
    if (nesting == 0)
      {
	int ch = skipHSpace();
	if (ch < 0 || ch == '\n' || ch == '\r')
	  return exp;
	unread(ch);
      }
    */
    getRawToken();
    return exp;
  }

  public Expression parseIfExpr()
      throws java.io.IOException, SyntaxException
  {
    getRawToken();
    nesting++;
    Expression cond = parseExpr();
    if (curToken != ')')
      return syntaxError("missing ')' after 'if (EXPR'");
    getRawToken();
    if (curToken != NCNAME_TOKEN
	|| tokenBufferLength != 4
	|| ! new String(tokenBuffer, 0, 4).equalsIgnoreCase("then"))
      return syntaxError("missing 'then'");
    getRawToken();
    Expression thenPart = parseExpr();
    if (curToken != NCNAME_TOKEN
	|| tokenBufferLength != 4
	|| ! new String(tokenBuffer, 0, 4).equalsIgnoreCase("else"))
      return syntaxError("missing 'else'");
    getRawToken();
    nesting--;
    Expression elsePart = parseExpr();
    return new IfExp(cond, thenPart, elsePart);
  }

  public boolean match(String word)
  {
    if (curToken != NCNAME_TOKEN)
      return false;
    int len = word.length();
    if (tokenBufferLength != len)
      return false;
    for (int i = len;  --i >= 0; )
      {
	char cs = word.charAt(i);
	char cb = tokenBuffer[i];
	if (cs != cb && cs != Character.toLowerCase(cb))
	  return false;
      }
    return true;
  }

  public Expression parseFLWRExpression (boolean isFor,
					 String name, Expression init)
      throws java.io.IOException, SyntaxException
  {
    ScopeExp sc;
    Expression[] inits = { init };
    if (isFor)
      {
	LambdaExp lexp = new LambdaExp();
	lexp.min_args = 1;
	lexp.max_args = 1;
	sc = lexp;
      }
    else
      {
	LetExp let = new LetExp(inits);
	sc = let;
      }
    Declaration decl = sc.addDeclaration(name);
    if (isFor)
      decl.noteValue (null);  // Does not have a known value.
    parser.push(sc);
    Expression body;
    if (curToken == ',')
      {
	int next = skipSpace();
	if (next != '$')
	  return syntaxError("missin $NAME after ','");
	body = parseFLWRExpression(isFor);
      }
    else
      {
	Expression cond;
	if (match("where"))
	  {
	    cond = parsePrimaryExpr();   // FIXME
	  }
	else
	  cond = null;
	if (! match("return"))
	  return syntaxError("missing 'return' clause");
	getRawToken();
	body = parseExpr();
	if (cond != null)
	  {
	    body = new IfExp(cond, body, QuoteExp.voidExp);
	  }
      }
    if (isFor)
      {
	((LambdaExp) sc).body = body;
	Expression[] args = { sc, inits[0]};  // SIC
	return new ApplyExp(makeFunctionExp("gnu.kawa.functions.ValuesMap",
					    "valuesMap"),
			    args);
      }
    else
      ((LetExp) sc).setBody(body);
    return sc;

  }

  /** Parse a let- or a for-expression.
   * Assume the 'let'/'for'-token has been seen, and we've read '$'. */
  public Expression parseFLWRExpression (boolean isFor)
      throws java.io.IOException, SyntaxException
  {
    getRawToken();
    String name;
    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
      name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    else
      return syntaxError("missing Variable token:"+curToken);
    getRawToken();
    ScopeExp sc;
    if (isFor)
      {
	char ch;
	if (! match("in"))
	  return syntaxError("missing 'in' in 'for' clause");
      }
    else
      {
	if (curToken != ':'
	    || getRawToken() != OP_EQU)
	  return syntaxError("missing ':=' in 'let' clause - "+curToken);
      }
    getRawToken();
    Expression value = parseExpr();
    return parseFLWRExpression(isFor, name, value);
  }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    return parse(null);
  }

  Parser parser;

  /** Parse an expression.
   * Return null on EOF. */
  public Expression parse(Parser parser)
      throws java.io.IOException, SyntaxException
  {
    this.parser = parser;
    if (getRawToken() == EOF_TOKEN)
      return null;
    Expression exp = parseExpr();
    if (curToken == EOL_TOKEN)
      unread('\n');
    return exp;
  }

  public final static String[] axisNames = new String[COUNT_OP_AXIS];
  static
  {
    axisNames[AXIS_ANCESTOR] = "ancestor";
    axisNames[AXIS_ANCESTOR_OR_SELF] = "ancestor-or-self";
    axisNames[AXIS_ATTRIBUTE] = "attribute";
    axisNames[AXIS_CHILD] = "child";
    axisNames[AXIS_DESCENDANT] = "descendant";
    axisNames[AXIS_DESCENDANT_OR_SELF] = "descendant-or-self";
    axisNames[AXIS_FOLLOWING] = "following";
    axisNames[AXIS_FOLLOWING_SIBLING] = "following-sibling";
    axisNames[AXIS_NAMESPACE] = "namespace";
    axisNames[AXIS_PARENT] = "parent";
    axisNames[AXIS_PRECEDING] = "preceding";
    axisNames[AXIS_PRECEDING_SIBLING] = "preceding-sibling";
    axisNames[AXIS_SELF] = "self";
  }
    
  public static Expression makeFunctionExp(String className, String name)
  {
    return makeFunctionExp(className,
			   Compilation.mangleNameIfNeeded(name),
			   name);
  }

  public static Expression makeFunctionExp(String className,
					   String fieldName, String name)
  {
    try
      {
	Class cls = Class.forName(className);
	java.lang.reflect.Field fld = cls.getDeclaredField(fieldName);
	Procedure proc = (Procedure) fld.get(null);
	//return new QuoteExp(proc);

	gnu.bytecode.ClassType type = gnu.bytecode.ClassType.make(className);
	gnu.bytecode.Field procField = type.getDeclaredField(fieldName);
	Declaration decl = new Declaration(name, procField);
	decl.noteValue(new QuoteExp(proc));
	decl.setFlag(Declaration.IS_CONSTANT);
	return new ReferenceExp(name, decl);
      }
    catch (Exception ex)
      {
	throw new WrappedException(ex);
      }
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
  {
    error(message);
    return new ErrorExp (message);
  }

}

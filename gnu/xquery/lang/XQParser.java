// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.xquery.util.*;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.text.*;
import gnu.expr.*;
import java.util.Vector;
import java.util.Hashtable;
import gnu.kawa.xml.*;
import gnu.xml.QName;
import gnu.bytecode.Type;

/** A class to read xquery forms. */

public class XQParser extends LispReader // should be extends Lexer
{
  int curToken;
  Object curValue;

  public static final gnu.kawa.reflect.InstanceOf instanceOf
  = new gnu.kawa.reflect.InstanceOf(XQuery.getInstance(), "instance");

  int nesting;

  /** Skip whitespace.
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

  final int skipSpaceOrComment()
    throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
	int ch = read();
	if (ch == '{')
	  {
	    ch = read();
	     if (ch != '-')
	       {
		 unread(ch);
		 return '{';
	       }
	    ch = read();
	     if (ch != '-')
	       {
		 unread(ch);
		 unread('-');
		 return '{';
	       }
	     skipComment();
	  }
	else if (ch < 0 || ! Character.isWhitespace((char) ch))
	  return ch;
      }
  }

  final void skipComment()
    throws java.io.IOException, SyntaxException
  {
    int seenDashes = 0;
    int startLine = getLineNumber() + 1;
    for (;;)
      {
	int ch = read();
	if (ch == '-')
	  seenDashes++;
	else if (ch == '}' && seenDashes >= 2)
	  return;
	else if (ch < 0)
	  eofError("non-terminated comment starting at line "+startLine);
	else
	  seenDashes = 0;
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

  /** Do skipSpace followed by unread to find next non-space character. */
  final int peekNonSpace(String message)
    throws java.io.IOException, SyntaxException
  {
    int ch = skipSpaceOrComment();
    if (ch < 0)
      eofError(message);
    unread(ch);
    return ch;
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

  /** A non-qualified (simple) name (NCName) followed by a colon.
   * The colon is not followed by another NCNAME (in which it would
   * be a QNAME_TOKEN instead).
   * The tokenBuffer contains the name (which does not contain the ':'). */
  static final int NCNAME_COLON_TOKEN = 'C';

  /** A Qualified name (QName).
   * The tokenBuffer contains the full name, which contains one ':'. */
  static final int QNAME_TOKEN = 'Q';

  static final int ARROW_TOKEN = 'R';

  /* FuncName including following '('). */
  static final int FNAME_TOKEN = 'F';

  static final int DEFINE_TOKEN = 'N';

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
  static final int OP_WHERE     = 196;
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
  static final int OP_UNION     = 200 + 24 + 2; // 'union'

  static final int OP_NODE = 231; // 'node' followed by '('
  static final int OP_TEXT = 232; // 'text' followed by '('
  
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
	    if (nesting <= 0)
	      return curToken = EOL_TOKEN;
	  }
	else if (next == '{')
	  {
	    next = read();
	    if (next != '-')
	      {
		unread(next);
		return curToken = '{';
	      }
	    next = read();
	    if (next != '-')
	      {
		// FIXME backup 2 chars. Can fix using special token for '{-'.
		unread();
		unread();
		return curToken = '{';
	      }
	    skipComment();
	  }
	else if (next != ' ' && next != '\t')
	  break;
      }
    tokenBufferLength = 0;
    char ch = (char) next;
    switch (ch)
      {
      case '(':  case ')':  case '[':  case ']':  case '}':
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
	    else
	      {
		if (next != ':')
		    ch = NCNAME_TOKEN;
		else
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
		      ch = NCNAME_COLON_TOKEN;
		  }
		unread(next);
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

  public void appendNamedEntity(String name, StringBuffer sbuf)
  {
    name = name.intern();
    char ch = '?';
    if (name == "lt")
      ch = '<';
    else if (name == "gt")
      ch = '>';
    else if (name == "amp")
      ch = '&';
    else if (name == "quot")
      ch = '"';
    else if (name == "apos")
      ch = '\'';
    else
      error("unknown enity reference: '"+name+"'");
    sbuf.append(ch);
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
	else if (len == 5)
	  {
	    if (match("where"))
	      curToken = OP_WHERE;
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
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next == '(')
	  {
	    int token = FNAME_TOKEN;
	    switch (tokenBuffer[0])
	      {
	      case 'i':
		if (match("if"))
		  {
		    unread();
		    return curToken;
		  }
		break;
	      case 'n':
		if (match("node")) token = OP_NODE;
		break;
	      case 't':
		if (match("text")) token = OP_TEXT;
		break;
	      }
	    return curToken = token;
	  }
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	curValue = name;
	if ((next == 'f' || next == 'F') && "define".equals(name))
	  {
	    unread();
	    return curToken = DEFINE_TOKEN;
	  }
	if (next >= 0)
	  unread();
	return curToken;
      }
    if (curToken == NCNAME_COLON_TOKEN)
      {
	int next = read();
	if (next == ':') // We've seen an Axis specifier.
	  {
	    // match axis name
	    String name
	      = new String(tokenBuffer, 0, tokenBufferLength).intern();
	    int i;
	    for (i = COUNT_OP_AXIS;  --i >= 0; )
	      if (axisNames[i] == name)
		break;
	    if (i >= 0)
	      curToken = (char) (OP_AXIS_FIRST + i);
	    else
	      error("unknown axis name '" + name + '\'');
	    curValue = name;
	  }
	else
	  unread(next);
      }
    return curToken;
  }


 public XQParser (InPort port)
  {
    super(port);
    nesting = 1;
  }

  public XQParser(InPort port, SourceMessages messages)
  {
    super(port, messages);
    nesting = 1;
  }

  public void setInteractive(boolean v)
  {
    if (interactive != v)
      if (v) nesting--; else nesting++;
    interactive = v;
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
		      ("gnu.kawa.functions.AppendValues", "appendValues"),
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
      case OP_EQU:
	func = makeFunctionExp("gnu.xquery.util.Compare", "=");
	break;
      case OP_NEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", "!=");
	break;
      case OP_LSS:
	func = makeFunctionExp("gnu.xquery.util.Compare", "<");
	break;
      case OP_LEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", "<=");
	break;
      case OP_GRT:
	func = makeFunctionExp("gnu.xquery.util.Compare", ">");
	break;
      case OP_GEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", ">=");
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

  public Expression parseElementType()
      throws java.io.IOException, SyntaxException
  {
    QName qname = parseNameTest(defaultNamespace);
    getRawToken();
    return new QuoteExp(new ElementType(qname));
  }

  public Expression parseDataType()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String tname = new String(tokenBuffer, 0, tokenBufferLength);
	getRawToken();
	if ("element".equalsIgnoreCase(tname))
	  {
	    return parseElementType();
	  }
	Type type = kawa.standard.Scheme.getNamedType(tname);
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
	      exp = new IfExp(booleanValue(exp), exp2, QuoteExp.falseExp);
	    else if (token == OP_OR)
	      exp = new IfExp(booleanValue(exp), QuoteExp.trueExp, exp2);
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

  QName parseNameTest(String defaultNamespaceUri)
      throws java.io.IOException, SyntaxException
  {
    String local = null, uri = null;
    if (curToken == QNAME_TOKEN)
      {
	int colon = tokenBufferLength;
	while (tokenBuffer[--colon] != ':') ;
	String prefix = new String(tokenBuffer, 0, colon);
	colon++;
	local = new String(tokenBuffer, colon,
				  tokenBufferLength - colon);
	uri = (String) namespaces.get(prefix);
	if (uri == null)
	  syntaxError("unknown namespace '" + prefix + "'");
      }
    else if (curToken == OP_MUL)
      {
	int next = read();
	if (next != ':')
	  unread(next);
	else
	  {
	    next = read();
	    if (next < 0)
	      eofError("unexpected end-of-file after '*:'");
	    if (isNameStart((char) next))
	      {
		unread();
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  syntaxError("invalid name test");
		else
		  local = new String(tokenBuffer, 0, tokenBufferLength);
	      }
	    else if (next != '*')
	      syntaxError("missing local-name after '*:'");
	  }
      }
    else if (curToken == NCNAME_TOKEN)
      {
	local = new String(tokenBuffer, 0, tokenBufferLength);
	uri = defaultNamespaceUri;
      }
    else if (curToken == NCNAME_COLON_TOKEN)
      {
	String prefix = new String(tokenBuffer, 0, tokenBufferLength);
	int next = read();
	if (next != '*')
	  {
	    syntaxError("invalid characters after 'NCName:'");
	    return QName.make(defaultNamespaceUri, prefix);
	  }
	uri = (String) namespaces.get(prefix);
	if (uri == null)
	  syntaxError("unknown namespace '" + prefix + "'");
	local = null;
      }
    return QName.make(uri, local);
  }

  Expression parseNodeTest(int axis)
      throws java.io.IOException, SyntaxException
  {
    Declaration dotDecl = parser.lookup("dot", -1);
    Expression dot = new ReferenceExp("dot", dotDecl);
    int token = peekOperand();
    /*
    if (token == NCNAME_TOKEN || token == QNAME_TOKEN 
	|| token == NCNAME_COLON_TOKEN || token == OP_MUL)
      {
	QName qname = parseNameTest(defaultNamespace; // FIXME null if attribute
      }
    else
    */
    if (token == FNAME_TOKEN)
      {
      }

    if (curToken == '@' && axis < 0)
      {
	getRawToken();
	axis = AXIS_ATTRIBUTE;
      }
    
    Expression exp;
    if ((axis < 0 || axis == AXIS_CHILD || axis == AXIS_DESCENDANT)
	&&
	(curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN 
	 || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL
	 || curToken == OP_NODE || curToken == OP_TEXT))
      {
	Object predicate;
	if (curToken == OP_NODE || curToken == OP_TEXT)
	  {
	    if (curToken == OP_NODE)
	      {
		predicate = new NodeType("node");
	      }
	    else // if (curToken == OP_TEXT)
	      {
		predicate = new NodeType("text", NodeType.TEXT_OK);
	      }
	    if (getRawToken() != ')')
	      return syntaxError("missing '()' after node test");
	  }
	else
	  {
	    QName qname = parseNameTest(defaultNamespace);
	    predicate = new ElementType(qname);
	  }
	Expression[] args = { dot, new QuoteExp(predicate) };
	Expression func
	  = axis == AXIS_DESCENDANT ? funcNamedDescendants : funcNamedChildren;
	exp = new ApplyExp(func, args);
      }
    else if (axis == AXIS_ATTRIBUTE)
      {
	if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN
	    || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL)
	  {
	    QName qname = parseNameTest(null);
	    Expression[] args = { dot, new QuoteExp(qname), };
	    exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.NamedAttributes", "namedAttributes"),
			       args);
	  }
	else
	  return syntaxError("missing name or '*' after '@' or attribute::");
      }
    else if (axis >= 0)
      return syntaxError("unsupported axis '"+axisNames[axis]+"::'");
    else
      return null;
    getRawToken();
    return exp;
  }

  Expression parseRelativePathExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseStepExpr();
    while (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	int op = curToken;
	boolean descendants = curToken == SLASHSLASH_TOKEN;
	getRawToken();

	LambdaExp lexp = new LambdaExp(1);
	Declaration decl = lexp.addDeclaration("dot");
	decl.setFlag(Declaration.IS_SINGLE_VALUE);
	decl.noteValue (null);  // Does not have a known value.
	parser.push(lexp);
	lexp.body = parseStepExpr();
	parser.pop(lexp);

	boolean handled = false;

	if (lexp.body instanceof ApplyExp)
	  {
	    // Optimize the case of a simple name step.
	    ApplyExp aexp = (ApplyExp) lexp.body;
	    Expression func = aexp.getFunction();
	    Expression[] args = aexp.getArgs();
	    if (func == funcNamedChildren && args.length==2
		&& args[0] instanceof ReferenceExp
		&& ((ReferenceExp) args[0]).getBinding() == decl)
	      {
		args[0] = exp;
		if (descendants)
		  func = funcNamedDescendants;
		exp = new ApplyExp (func, args);
		handled = true;
	      }
	    else if (func == funcValuesFilter && args.length==2
		     && args[0] instanceof ApplyExp
		     && descendants)
	      {
		ApplyExp xapp = (ApplyExp) args[0];
		Expression[] xargs = xapp.getArgs();
		if (xapp.getFunction() == funcNamedChildren
		    && xargs.length == 2
		    && ((ReferenceExp) xargs[0]).getBinding() == decl)
		  {
		    xapp.setFunction(funcNamedDescendants);
		  }
	      }
	  }

	if (! handled)
	  {
	    Expression[] args = new Expression[] { lexp, exp };
	    Expression func = makeFunctionExp("gnu.kawa.functions.ValuesMap",
				   "valuesMap");
	    exp = new ApplyExp(func, args);
	  }
      }
    return exp;
  }

  Expression parseStepExpr()
      throws java.io.IOException, SyntaxException
  {
    int axis = peekOperand() - OP_AXIS_FIRST;
    if (axis  >= 0 && axis < COUNT_OP_AXIS)
      {
	getRawToken();
	return parseStepQualifiers(parseNodeTest(axis));
      }
    else
      return parseOtherStepExpr();
  }

  Expression parseStepQualifiers(Expression exp)
    throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
	if (curToken == '[')
	  {
	    int startLine = getLineNumber() + 1;
	    int startColumn = getColumnNumber() + 1;
	    getRawToken();
	    LambdaExp lexp = new LambdaExp(1);
	    lexp.setFile(getName());
	    lexp.setLine(startLine, startColumn);
	    parser.push(lexp);
	    Declaration dot = lexp.addDeclaration("dot");
	    dot.noteValue(null);
	    Expression cond = parseExpr();
	    cond.setFile(getName());
	    cond.setLine(startLine, startColumn);
	    parser.pop(lexp);
	    lexp.body = cond;
	    if (curToken != ']')
	      return syntaxError("missing ']'");
	    getRawToken();
	    Expression[] args = { exp, lexp };
	    exp = new ApplyExp(funcValuesFilter, args);
	  }
	/*
	else if (curToken == ARROW_TOKEN)
	  ...;
	*/
	else
	  return exp;
      }
  }

  /* Parse an OtherStepExpr.
   */
  Expression parseOtherStepExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression e = parsePrimaryExpr();
    e = parseStepQualifiers(e);
    return e;
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
   *   '\"' if parsing AttributeContent depending on the starting quote
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
	    result.addElement(new QuoteExp(sbuf.toString()));
	    sbuf.setLength(0);
	  }
	if (next < 0)
	  eofError("unexpected end-of-file");
	if (next == '{')
	  {
	    next = read();
	    if (next == '{')
	      sbuf.append('{');
	    else
	      {
		unread(next);
		Expression exp = parseEnclosedExpr();
		if (delimiter != '<')
		  exp = stringValue(exp);
		result.addElement(exp);
	      }
	  }
	else if (next == '}')
	  {
	    next = read();
	    if (next == '}')
	      sbuf.append('}');
	    else
	      {
		error("unexpected '}' in element content");
		unread(next);
	      }
	  }
	else if (next == '<' && delimiter == '<')
	  {
	    next = read();
	    if (next == '/')
	      {
		break;
	      }
	    unread(next);
	    getRawToken();
	    Expression exp = parseElementConstructor();
	    if (delimiter != '<')
	      exp = stringValue(exp);
	    result.addElement(exp);
	  }
	else if (next == delimiter)
	  break;
	else if (next == '&')
	  {
	    StringBuffer cbuf = new StringBuffer(30);
	    int base = 0;
	    next = read();
	    if (next == '#')
	      {
		next = read();
		if (next == 'x')
		  {
		    base = 16;
		    next = read();
		  }
		base = 10;
	      }
	    while (next >= 0)
	      {
		char ch = (char) next;
		if (! isNamePart(ch))
		  break;
		cbuf.append(ch);
		next = read();
	      }
	    if (next != ';')
	      error("invalid entity/character reference");
	    else
	      {
		String ref = cbuf.toString();
		if (base == 0)
		  appendNamedEntity(ref, sbuf);
		else
		  {
		    try
		      {
			sbuf.append((char) Integer.parseInt(ref, base));
		      }
		    catch (NumberFormatException ex)
		      {
			error("invalid character references");
		      }
		  }
	      }
	  }
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
    peekNonSpace("unexpected end-of-file after '{'");
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() + 1;
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
    exp.setFile(getName());
    exp.setLine(startLine, startColumn);
    if (curToken != '}')
      return syntaxError("missing '}'");
    return exp;
  }

  /** Coerce the value of an expresison to a string value. */
  public static Expression stringValue(Expression exp)
  {
    Expression[] args = { exp };
    Expression string
      = makeFunctionExp("gnu.xquery.util.StringValue", "string");
    return new ApplyExp(string, args);
  }

  /** Coerce the value of an expresison to a boolean value. */
  public static Expression booleanValue(Expression exp)
  {
    Expression[] args = { exp };
    Expression string
      = makeFunctionExp("gnu.xquery.util.BooleanValue", "booleanValue");
    return new ApplyExp(string, args);
  }

  Expression parseNameSpec(String defaultNamespaceUri, boolean attribute)
      throws java.io.IOException, SyntaxException
  {
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	QName qname = curToken == NCNAME_TOKEN
	  ? QName.make(defaultNamespaceUri, name)
	  : parseNameTest(defaultNamespaceUri);
	return new QuoteExp(attribute
			    ? (Object) AttributeConstructor.make(name, qname)
			    : (Object) ElementConstructor.make(name, qname));
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
    Expression element = parseNameSpec(defaultNamespace, false);
    vec.addElement(element);
    if (element == null)
      return syntaxError("missing NameSpec");
    Expression[] args;
    int ch;
    for (;;)
      {
	ch = skipSpace();
	if (ch < 0 || ch == '>' || ch == '/')
	  break;
	unread(ch);
	getRawToken();
	int vecSize = vec.size();
	Expression makeAttr = parseNameSpec(null, true);
	if (makeAttr == null)
	  break;
	if (! (makeAttr instanceof QuoteExp)
	    || !
	    (((QuoteExp)makeAttr).getValue() instanceof AttributeConstructor))
	  {
	    vec.addElement(makeAttr);
	    makeAttr = makeFunctionExp("gnu.xquery.util.MakeAttribute", "makeAttribute");
	  }
	ch = skipSpace();
	if (ch != '=')
	  return syntaxError("missing '=' after attribute");
	ch = skipSpace();
	if (ch == '{')
	  vec.addElement(stringValue(parseEnclosedExpr()));
	else
	  parseContent(ch, vec);
	args = new Expression[vec.size() - vecSize];
	for (int i = args.length;  --i>= 0; )
	  args[i] = (Expression) vec.elementAt(vecSize + i);
	vec.setSize(vecSize);
	vec.addElement(new ApplyExp(makeAttr, args));
      }
    boolean empty = false;
    if (ch == '/')
      {
	ch = read();
	if (ch == '>')
	  empty = true;
	else
	  unread(ch);
      }
    if (! empty)
      {
	if (ch != '>')
	  return syntaxError("missing '>' after start element");
	parseContent('<', vec);
	ch = skipSpace();
	if (ch >= 0 && ch != '>')
	  {
	    unread(ch);
	    getRawToken();
	    if (curToken != NCNAME_TOKEN && curToken != QNAME_TOKEN)
	      return syntaxError("invalid tag syntax after '</'");
	    if (! (element instanceof QuoteExp))
	      return syntaxError("'<{'expression'}>' must be closed by '</>'");
	    String tag = new String(tokenBuffer, 0, tokenBufferLength);
	    Object start = ((QuoteExp) element).getValue();
	    String startTag = start instanceof ElementConstructor
	      ? ((ElementConstructor) start).getXmlName()
	      : start.toString();
	    if (! (tag.equals(startTag)))
	      return syntaxError("'<"+startTag+">' closed by '</"+tag+">'");
	    ch = skipSpace();
	  }
	if (ch != '>')
	  return syntaxError("missing '>' after end element");
      }
    args = new Expression[vec.size()];
    vec.copyInto(args);
    nesting--;
    return new ApplyExp(makeFunctionExp("gnu.xquery.util.MakeElement", "makeElement"),
			args);
  }

  Expression parseExprSequence()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == ')' || curToken == EOF_TOKEN)
      return QuoteExp.voidExp;
    Expression exp = null;
    for (;;)
      {
	Expression exp1 = parseExpr();
	exp = exp == null ? exp1 : makeExprSequence(exp, exp1);
	if (curToken == ')' || curToken == EOF_TOKEN)
	  break;
	if (nesting == 0 && curToken == EOL_TOKEN)
	  return exp;
	if (curToken != ',')
	  return syntaxError("missing ')' - saw "+new String(tokenBuffer, 0, tokenBufferLength)+" @:"+getColumnNumber());
	getRawToken();
      }
    return exp;
  }

  Expression parseTypeSwitch()
    throws java.io.IOException, SyntaxException
  {
    getRawToken();
    Expression selector = parseExpr();
    if (curToken != ')')
      return syntaxError("missing ')' after 'typeswitch' selector");
    getRawToken();
    Object varName = null;
    Vector vec = new Vector();
    vec.addElement(selector);
    if (match("as"))
      {
	getRawToken();
	if (curToken == '$')
	  {
	    getRawToken();
	    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
	      varName
		= new String(tokenBuffer, 0, tokenBufferLength).intern();
	  }
	if (varName == null)
	  return syntaxError("missing Variable after 'as' clause");
	getRawToken();
      }
    String argName = varName == null ? "$arg$" : varName.toString();
    while (match("case"))
      {
	getRawToken();
	Expression caseType = parseDataType();
	LambdaExp lexp = new LambdaExp(1);
	Declaration decl = lexp.addDeclaration(argName,  // FIXME cast
					       (Type) ((QuoteExp) caseType).getValue());

	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'case'");
	parser.push(lexp);
	Expression caseExpr = parseExpr();
	lexp.body = caseExpr;
	parser.pop(lexp);
	vec.addElement(lexp);
      }
    LambdaExp lexp = new LambdaExp(0);
    if (match("default"))
      {
	getRawToken();
	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'default'");
	parser.push(lexp);
	Expression defaultExpr = parseExpr();
	lexp.body = defaultExpr;
	parser.pop(lexp);
      }
    else
      {
	lexp.body = QuoteExp.voidExp;
	error('w', "no 'default' clause in 'typeswitch'");
      }
    vec.addElement(lexp);
    Expression[] args = new Expression[vec.size()];
    vec.copyInto(args);
    return new ApplyExp(makeFunctionExp("gnu.kawa.reflect.TypeSwitch",
					"typeSwitch"),
			args);
  }

  /**
   * Try to parse a PrimaryExpr.
   * @return an Expression, or null if no PrimaryExpr was seen.
   */
  Expression parseMaybePrimaryExpr()
      throws java.io.IOException, SyntaxException
  {
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber()+ 1;
    int token = peekOperand();
    Expression exp;
    int c1, c2, c3;
    if (token == '(')
      {
	getRawToken();
	nesting++;
	exp = parseExprSequence();
	nesting--;
	if (curToken == EOF_TOKEN)
	  eofError("missing ')' - unexpected end-of-file");
      }
    else if (token == OP_LSS)
      {
	startColumn--;  // Subtract 1 for '<'.
	getRawToken();
	if (curToken == '/')
	  {
	    getRawToken();
	    String msg;
	    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN
		|| curToken == NCNAME_COLON_TOKEN)
	      msg = "saw end tag '</" + new String(tokenBuffer, 0, tokenBufferLength) + ">' not in an element constructor";
	    else
	      msg = "saw end tag '</' not in an element constructor";
	    exp = syntaxError(msg);
	    while (curToken != OP_GRT && curToken != EOF_TOKEN && curToken != EOL_TOKEN)
	      getRawToken();
	    return exp;
	  }
	InPort port = (InPort) getPort();
	char saveReadState = port.readState;
	try
	  {
	    port.readState = '<';
	    exp = parseElementConstructor();
	    exp.setFile(getName());
	    exp.setLine(startLine, startColumn);
	  }
	finally
	  {
	    port.readState = saveReadState;
	  }
      }
    else if (token == STRING_TOKEN)
      {
	exp = new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength));
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
	exp = null;
	if (decl == null && Compilation.generateServletDefault)
	  {
	    if (name == "request")
	      exp = makeFunctionExp("gnu.kawa.servlet.GetRequest",
				    "getRequest");
	    if (name == "response")
	      exp = makeFunctionExp("gnu.kawa.servlet.GetResponse",
				    "getResponse");
	    if (exp != null)
	      exp = new ApplyExp(exp, Expression.noExpressions);
	  }
	if (exp == null)
	  exp = new ReferenceExp(name, decl);
      }
    else if (token == FNAME_TOKEN)
      {
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	if (name.equals("typeswitch"))
	  return parseTypeSwitch();;
	startColumn -= tokenBufferLength;
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
	Declaration decl = parser.lookup(name, Interpreter.FUNCTION_NAMESPACE);
	exp = new ApplyExp(new ReferenceExp(name, decl), args);
	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
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
		c2 = tokenBuffer[1];
		c3 = tokenBuffer[2];
		if (c1 == 'l' && c2 == 'e' && c3 == 't')
		  forOrLet = 0;
		else if (c1 == 'f' && c2 == 'o' && c3 == 'r')
		  forOrLet = 1;
	      }
	    if (forOrLet < 0)
	      throw new Error/*return syntaxError*/("invalid syntax - variable following name");
	    exp = parseFLWRExpression(forOrLet > 0);
	    exp.setFile(getName());
	    exp.setLine(startLine, startColumn - 3);
	    return exp;
	  }
	else if (next == '(' && tokenBufferLength == 2
		 && tokenBuffer[0] == 'i'
		 && tokenBuffer[1] == 'f')
	  {
	    return parseIfExpr();
	  }
	else
	  {
	    if (next >= 0)
	      unread();
	    return parseNodeTest(-1);
	  }
      }
    else if (token == OP_MUL || token == NCNAME_COLON_TOKEN || token == '@'
	     || token == OP_NODE || token == OP_TEXT)
      {
	return parseNodeTest(-1);
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
    return new IfExp(booleanValue(cond), thenPart, elsePart);
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
	if (cs != cb)
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
	LambdaExp lexp = new LambdaExp(1);
	sc = lexp;
      }
    else
      {
	LetExp let = new LetExp(inits);
	sc = let;
      }
    Declaration decl = sc.addDeclaration(name);
    if (isFor)
      {
	decl.noteValue (null);  // Does not have a known value.
	decl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    parser.push(sc); // FIXME where is matching pop?
    Expression body;
    if (curToken == ',')
      {
	int next = skipSpaceOrComment();
	if (next != '$')
	  return syntaxError("missing $NAME after ','");
	body = parseFLWRExpression(isFor);
      }
    else
      {
	Expression cond;
	nesting++;
	if (curToken == OP_WHERE)
	  {
	    getRawToken();
	    cond = parseBinaryExpr(priority(OP_OR));
	  }
	else if (match("where"))
	  {
	    cond = parseBinaryExpr(priority(OP_OR));
	  }
	else
	  cond = null;
	nesting--;
	boolean sawReturn = match("return");
	if (! sawReturn && ! match("let") && ! match("for"))
	  return syntaxError("missing 'return' clause");
	peekNonSpace("unexpected eof-of-file after 'return'");
	int bodyLine = getLineNumber() + 1;
	int bodyColumn = getColumnNumber() + 1;
	if (sawReturn)
	  getRawToken();
	body = parseExpr();
	if (cond != null)
	  {
	    body = new IfExp(booleanValue(cond), body, QuoteExp.voidExp);
	  }
	body.setFile(getName());
	body.setLine(bodyLine, bodyColumn);
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
    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN
	|| curToken == NCNAME_COLON_TOKEN)
      name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    else
      return syntaxError("missing Variable token:"+curToken);
    if (curToken == NCNAME_COLON_TOKEN)
      curToken = ':';
    else
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
    nesting++;
    Expression value = parseBinaryExpr(priority(OP_OR));
    nesting--;
    return parseFLWRExpression(isFor, name, value);
  }

  public Expression parseFunctionDefinition(int declLine, int declColumn)
      throws java.io.IOException, SyntaxException
  {
    if (curToken != QNAME_TOKEN && curToken != NCNAME_TOKEN)
      return syntaxError("missing function name");
    String name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    getRawToken();
    if (curToken != '(')
      return syntaxError("missing parameter list:"+curToken);
    getRawToken();
    LambdaExp lexp = new LambdaExp();
    lexp.setFile(getName());
    lexp.setLine(declLine, declColumn);
    lexp.setName(name);
    Declaration decl = parser.currentScope().addDeclaration(name);
    decl.setCanRead(true);
    decl.setProcedureDecl(true);
    decl.setFile(getName());
    decl.setLine(declLine, declColumn);
    parser.push(lexp);
    if (curToken != ')')
      {
	for (;;)
	  {
	    if (curToken != '$')
	      error("missing '$' before parameter name");
	    else
	      getRawToken();
	    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
	      {
		String pname
		  = new String(tokenBuffer, 0, tokenBufferLength).intern();
		Declaration param = lexp.addDeclaration(pname);
		//parser.push(param);
		getRawToken();
	      }
	    lexp.min_args++;
	    lexp.max_args++;
	    if (curToken == ')')
	      break;
	    if (curToken != ',')
	      return syntaxError("missing ',' in parameter list");
	    getRawToken();
	  }
      }
    getRawToken();
    lexp.body = parseEnclosedExpr();
    parser.pop(lexp);
    SetExp sexp = new SetExp (name, lexp);
    sexp.setDefining (true);
    sexp.binding = decl;
    decl.noteValue(lexp);
    return sexp;
  }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    return parse(null);
  }

  Parser parser;

  String defaultNamespace = "";
  Hashtable namespaces = new Hashtable(50);

  /** Parse an expression.
   * Return null on EOF. */
  public Expression parse(Parser parser)
      throws java.io.IOException, SyntaxException
  {
    this.parser = parser;
    int ch = skipSpace();
    if (ch < 0)
      return null;
    unread(ch);
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() + 1;

    // Handle Unix #!PROGRAM convention. */
    if (ch == '#' && startLine == 1 && startColumn == 1)
      {
	read();
	if ((ch = read()) != '!' || (ch = read()) != '/')
	  error("'#' is only allowed in initial '#!/PROGRAM'");
	while (ch != '\r' && ch != '\n' && ch >= 0)
	  ch = read();
      }

    if (getRawToken() == EOF_TOKEN)
      return null;
    peekOperand();
    if (curToken == DEFINE_TOKEN)
      {
	getRawToken();
	peekNonSpace("unexpected end-of-file after 'define'");
	int declLine = getLineNumber() + 1;
	int declColumn = getColumnNumber() + 1;
	if (match("function"))
	  getRawToken();
	else
	  error("'define' is not followed by 'function'");
	Expression exp = parseFunctionDefinition(declLine, declColumn);
	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
	return exp;
      }
    if (curToken == NCNAME_TOKEN
	&& "namespace".equals((String) curValue))
      {
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next))
	      {
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  return syntaxError("confused after seeing 'namespace'");
		String prefix = new String(tokenBuffer, 0, tokenBufferLength);
		getRawToken();
		if (curToken != OP_EQU)
		  return syntaxError("missing '=' in namespace declaration");
		getRawToken();
		if (curToken != STRING_TOKEN)
		  return syntaxError("missing uri namespace declaration");
		String uri = new String(tokenBuffer, 0, tokenBufferLength);
		namespaces.put(prefix, uri);
		return QuoteExp.voidExp;
	      }
	  }
      }
    if (curToken == NCNAME_TOKEN
	&& "default".equals((String) curValue))
      {
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next))
	      {
		getRawToken();
		curValue = new String(tokenBuffer, 0, tokenBufferLength);
		if (curToken != NCNAME_TOKEN ||
		    ! "namespace".equalsIgnoreCase((String) curValue))
		  return syntaxError("expected 'namespace' after 'default'");
		getRawToken();
		if (curToken != OP_EQU)
		  return syntaxError("missing '=' in namespace declaration");
		getRawToken();
		if (curToken != STRING_TOKEN)
		  return syntaxError("missing uri namespace declaration");
		String uri = new String(tokenBuffer, 0, tokenBufferLength);
		defaultNamespace = uri.toString();
		return QuoteExp.voidExp;
	      }
	  }
      }

    Expression exp = parseExprSequence();
    if (curToken == EOL_TOKEN)
      unread('\n');
    exp.setFile(getName());
    exp.setLine(startLine, startColumn);
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

  static final Expression funcNamedChildren
    = makeFunctionExp("gnu.kawa.xml.NamedChildren", "namedChildren");
  static final Expression funcNamedDescendants
    = makeFunctionExp("gnu.kawa.xml.NamedDescendants", "namedDescendants");
  static final Expression funcValuesFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "valuesFilter");

  public void error(String message)
  {
    super.error(message);
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message)
    throws java.io.IOException, SyntaxException
  {
    error(message);
    if (interactive)
      {
	curToken = 0;
	curValue = null;
	nesting = 0;
	for (;;)
	  {
	    int ch = read();
	    if (ch < 0)
	      break;
	    if (ch == '\r' || ch == '\n')
	      {
		unread(ch);
		break;
	      }
	  }
	throw new SyntaxException(getMessages());
      }
    return new ErrorExp (message);
  }

}

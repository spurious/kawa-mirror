// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
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
import gnu.bytecode.Type;
import gnu.kawa.reflect.OccurrenceType;
import gnu.kawa.functions.Convert;

/** A class to read xquery forms. */

public class XQParser extends LispReader // should be extends Lexer
{
  int curToken;
  Object curValue;
  XQuery interpreter;

  int seenPosition;
  int seenLast;

  public static boolean warnOldVersion;

  /** The inetrnal name of the variable containing '.', teh context node. */
  static final String DOT_VARNAME = "$dot$";

  /** The pseduo-function position() is mapped to a reference. */
  static final String POSITION_VARNAME = "$position$";

  /** The pseduo-function last() is mapped to a reference to this variable. */
  static final String LAST_VARNAME = "$last$";

  public static final gnu.kawa.reflect.InstanceOf instanceOf
  = new gnu.kawa.reflect.InstanceOf(XQuery.getInstance(), "instance");

  int nesting;

  boolean preserveBoundarySpace;

  /** Enter a nested expression.
   * This is used in interactive mode to control whether to continue
   * past end of line, depending on whether the expression is incomplete.
   * @parm promptChar Used in prompt string to indicate type of nesting.
   * @return The previous value of promptChar, to be passed to popNesting.
   */
  protected char pushNesting (char promptChar)
  {
    nesting++;
    InPort port = (InPort) getPort();
    char save = port.readState;
    port.readState = promptChar;
    return save;
  }

  /** Exit a nested expression, reversing pushNesting
   * @param save Saved values return by prior pushNEsting
   */
  protected void popNesting (char save)
  {
    InPort port = (InPort) getPort();
    port.readState = save;
    nesting--;
  }

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
	if (ch == '(')
	  {
	    if (! checkNext(':'))
	      return '(';
	    skipComment();
	  }
	else if (ch == '{')
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
	     skipOldComment();
	  }
	else if (ch < 0 || ! Character.isWhitespace((char) ch))
	  return ch;
      }
  }

  final void skipOldComment()
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

  final void skipComment()
    throws java.io.IOException, SyntaxException
  {
    int startLine = getLineNumber() + 1;
    int prev = 0;
    int commentNesting = 0;
    char saveReadState = pushNesting(':');
    for (;;)
      {
	int ch = read();
	if (ch == ':')
	  {
	    if (prev == '(')
	      {
		commentNesting++;
		ch = 0;
	      }
	  }
	else if (ch == ')' && prev == ':')
	  {
	    if (commentNesting == 0)
	      {
		popNesting(saveReadState);
		return;
	      }
	    --commentNesting;
	  }
	else if (ch < 0)
	  eofError("non-terminated comment starting at line "+startLine);
	prev = ch;
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
  static final int COLON_EQUAL_TOKEN = 'L'; // ":="
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

  static final int DECLARE_NAMESPACE_TOKEN = 'M'; // <"declare" "namespace">
  static final int DECLARE_XMLSPACE_TOKEN = 'S'; // <"declare" "xmlspace">
  static final int DEFAULT_ELEMENT_TOKEN = 'N'; // <"default" "element">
  static final int DEFAULT_FUNCTION_TOKEN = 'O'; // <"default" "function">
  static final int DECLARE_FUNCTION_TOKEN = 'P'; // <"declare" "function">
  static final int DECLARE_VARIABLE_TOKEN = 'V'; // <"declare" "variable">
  static final int DEFINE_QNAME_TOKEN = 'W'; // <"define" QName> - an error

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
  static final int OP_BASE      = 400;
  static final int OP_OR        = OP_BASE;          // 'or'
  static final int OP_AND       = OP_BASE + 4;      // 'and'
  static final int OP_EQU       = OP_BASE + 8;      // '='
  static final int OP_NEQ       = OP_BASE + 8 + 1;  // '!='
  static final int OP_INSTANCEOF= OP_BASE + 8 + 2;  // 'instanceof'
  static final int OP_RANGE_TO  = OP_BASE + 8 + 3;  // 'to'
  static final int OP_LSS       = OP_BASE + 12;     // '<'
  static final int OP_GRT       = OP_BASE + 12 + 1; // '>'
  static final int OP_LEQ       = OP_BASE + 12 + 2; // '<='
  static final int OP_GEQ       = OP_BASE + 12 + 3; // '>='
  static final int OP_IS        = OP_BASE + 16;     // 'is'
  static final int OP_ISNOT     = OP_BASE + 16 + 1; // 'isnot'
  static final int OP_GRTGRT    = OP_BASE + 16 + 2; // '>>'
  static final int OP_LSSLSS    = OP_BASE + 16 + 3; // '<<'
  static final int OP_ADD       = OP_BASE + 20;     // '+'
  static final int OP_SUB       = OP_BASE + 20 + 1; // '-'
  static final int OP_MUL       = OP_BASE + 24;     // '*'
  static final int OP_DIV       = OP_BASE + 24 + 1; // 'div'
  static final int OP_MOD       = OP_BASE + 24 + 2; // 'mod'
  static final int OP_INTERSECT = OP_BASE + 28;     // 'intersect'
  static final int OP_EXCEPT    = OP_BASE + 28 + 1; // 'except'
  static final int OP_UNION     = OP_BASE + 28 + 2; // 'union'

  static final int OP_NODE = 231; // 'node' followed by '('
  static final int OP_TEXT = 232; // 'text' followed by '('
  
  public static boolean isNameStart(char ch)
  {
    return Character.isLetter(ch) || ch == '_';
  }

  public static boolean isNamePart(char ch)
  {
    return Character.isUnicodeIdentifierPart(ch) || ch == '-' || ch == '.';
  }

  private int saveToken;
  private Object saveValue;

  public void mark ()
    throws java.io.IOException
  {
    super.mark();
    saveToken = curToken;
    saveValue = curValue;
  }

  public void reset()
    throws java.io.IOException
  {
    curToken = saveToken;
    curValue = saveValue;
    super.reset();
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
	else if (next == '(')
	  {
	    if (checkNext(':'))
	      skipComment();
	    else
	      return curToken = '(';
	  }
	else if (next == '{')
	  {
	    if (! checkNext('-'))
	      return curToken = '{';
	    next = read();
	    if (next != '-')
	      {
		// FIXME backup 2 chars. Can fix using special token for '{-'.
		unread();
		unread();
		return curToken = '{';
	      }
	    skipOldComment();
	  }
	else if (next != ' ' && next != '\t')
	  break;
      }
    tokenBufferLength = 0;
    char ch = (char) next;
    switch (ch)
      {
      case ')':  case '[':  case ']':  case '}':
      case '$':  case '@':  case ',':  case '?':
	break;
      case ':':
	if (checkNext('='))
	  ch = COLON_EQUAL_TOKEN;
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
	ch = checkNext('=') ? (char) OP_GEQ
	  : checkNext('>') ? (char) OP_GRTGRT : (char) OP_GRT;
	break;
      case '<':
	ch = checkNext('=') ? (char) OP_LEQ
	  : checkNext('<') ? (char) OP_LSSLSS : (char) OP_LSS;
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
	char saveReadState = pushNesting ((char) next);
	for (;;)
	  {
	    next = read();
	    if (next < 0)
	      eofError("unexpected end-of-file in string");
	    if (next == '&')
	      {
		parseEntityOrCharRef();
		continue;
	      }
	    else if (ch == next)
	      {
		next = read ();
		if (ch != next)
		  {
		    unread(next);
		    break;
		  }
	      }
	    tokenBufferAppend((char) next);
	  }
	popNesting(saveReadState);
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
		    else if (ch == '=')
		      {
			unread(ch);
			ch = NCNAME_TOKEN;
		      }
		    else
		      ch = NCNAME_COLON_TOKEN;
		  }
		unread(next);
	      }
	  }
	else if (ch >= ' ' && ch < 127)
	  syntaxError("invalid character '"+ch+'\'', 1);
	else
	  syntaxError("invalid character '\\u"+Integer.toHexString(ch)+'\'',
		      1);
      }
    curToken = ch;
    return ch;
  }

  public void appendNamedEntity(String name)
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
    tokenBufferAppend(ch);
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
		else if (c1 == 'i' && tokenBuffer[1] == 's')
		  curToken = OP_IS;
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
	    else if (match("isnot"))
	      curToken = OP_ISNOT;
	    else if (match("union"))
	      curToken = OP_UNION;
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

  /**
   * Internal method to match against double-lexeme tokens.
   * @param word0 expected previous word
   * @param word1 expected next word
   */
  private boolean lookingAt (String word0, String word1)
      throws java.io.IOException, SyntaxException
  {
    if (! word0.equals(curValue))
      return false;
    int i = 0;
    int len = word1.length();
    for (;; )
      {
	int ch = read();
	if (i == len)
	  {
	    if (ch < 0)
	      return true;
	    if ( ! isNamePart((char) ch))
	      {
		unread();
		return true;
	      }
	    i++;
	    break;
	  }
	if (ch < 0 || ch != word1.charAt(i++))
	  break;
      }
    port.skip(-i);
    return false;
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
	if (next == '(' && peek() != ':')
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
	switch (next)
	  {
	  case 'e':
	    if (lookingAt("default", /*"e"+*/ "lement"))
	      return curToken = DEFAULT_ELEMENT_TOKEN;
	    break;
	  case 'f':
	    if (lookingAt("declare", /*"f"+*/ "unction"))
	      return curToken = DECLARE_FUNCTION_TOKEN;
	    if (lookingAt("define", /*"f"+*/ "unction"))
	      {
		if (warnOldVersion)
		  error('w',
			"replace 'define function' by 'declare function'");
		return curToken = DECLARE_FUNCTION_TOKEN;
	      }
	    if (lookingAt("default", /*"f"+*/ "unction"))
	      return curToken = DEFAULT_FUNCTION_TOKEN;
	    break;
	  case 'n':
	    if (lookingAt("declare", /*"n"+*/ "amespace"))
	      return curToken = DECLARE_NAMESPACE_TOKEN;
	    break;
	  case 'v':
	    if (lookingAt("declare", /*"v"+*/ "ariable"))
	      return curToken = DECLARE_VARIABLE_TOKEN;
	    if (lookingAt("define", /*"v"+*/ "ariable"))
	      {
		if (warnOldVersion)
		  error('w',
			"replace 'define variable' by 'declare variable'");
		return curToken = DECLARE_VARIABLE_TOKEN;
	      }
	    break;
	  case 'x':
	    if (lookingAt("declare", /*"x"+*/ "mlspace"))
	      return curToken = DECLARE_XMLSPACE_TOKEN;
	    break;
	  }
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next) && curValue.equals("define"))
	      {
		getRawToken();
		curToken = DEFINE_QNAME_TOKEN;
	      }
	  }
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

  private void pushStandardNamespaces ()
  {
    namespaces.put("xml", "http://www.w3.org/XML/1998/namespace");
    namespaces.put("xs", "http://www.w3.org/2001/XMLSchema");
    namespaces.put("xsi", "http://www.w3.org/2001/XMLSchema-instance");
    namespaces.put("fn", "http://www.w3.org/2002/11/xquery-functions");
    namespaces.put("local", "http://www.w3.org/2003/08/xquery-local-functions");
  }

  public XQParser (InPort port)
  {
    this(port, null);
  }

  public XQParser(InPort port, SourceMessages messages)
  {
    super(port, messages);
    pushStandardNamespaces();
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

  private static final int priority(int opcode)
  {
    /* FIXME:  Future!
    switch (opcode)
      {
      case ',':
      case ')':
      case -1:
      case OP_OR:
	return OP_BASE >> 2;
      case OP_AND:
	return (OP_BASE + 4) >> 2;
      case OP_EQU:  case OP_NEQ:
      case OP_INSTANCEOF:  case OP_RANGE_TO:
	return (OP_BASE + 8) >> 2;
      case OP_LSS:  case OP_GRT:  case OP_LEQ:  case OP_GEQ:
	return (OP_BASE + 12) >> 2;
      case OP_IS:  case OP_ISNOT:
      case OP_GRTGRT:  case OP_LSSLSS:
	return (OP_BASE + 16) >> 2;
      case OP_ADD:  case OP_SUB:
	return (OP_BASE + 20) >> 2;
      case OP_MUL: case OP_DIV:  case OP_MOD:
	return (OP_BASE + 24) >> 2;
      case OP_INTERSECT:  case OP_EXCEPT:  case OP_UNION:
	return (OP_BASE + 28) >> 2;
      default:
	//System.err.println("unknown priorty for "+opcode);
	return 0;
      }
    */
    return opcode >> 2;
  }

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
      throws java.io.IOException, SyntaxException
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
      case OP_IS:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Eq", "is");
	break;
      case OP_ISNOT:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Ne", "isnot");
	break;
      case OP_GRTGRT:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Gr", ">>");
	break;
      case OP_LSSLSS:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Ls", "<<");
	break;
      case OP_RANGE_TO:
	func = makeFunctionExp("gnu.xquery.util.IntegerRange", "integerRange");
	break;
      case OP_UNION:
	func = makeFunctionExp("gnu.kawa.xml.UnionNodes", "unionNodes");
	break;
      default:
	return syntaxError("unimplemented binary op: "+op);
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

  private void  parseSimpleKindType ()
    throws java.io.IOException, SyntaxException
  {
    getRawToken();
    if (curToken == '(')
      {
	getRawToken();
	if (curToken == ')')
	  getRawToken();
	else
	  error("expected ')'");
      }
    else
      warnOldStyleKindTest();
  }

  public Type parseElementType ()
      throws java.io.IOException, SyntaxException
  {
    Symbol qname;
    if (curToken == '(')
      {
	getRawToken();
	if (curToken == ')')
	  {
	    qname = new Symbol(null);
	    getRawToken();
	  }
	else
	  {
	    qname = parseNameTest(defaultElementNamespace);
	    getRawToken();
	    if (curToken == ',')
	      {
		getRawToken();
		Symbol tname = parseNameTest(defaultElementNamespace);
		getRawToken();
	      }
	    if (curToken == ')')
	      getRawToken();
	    else
	      error("expected ')' after element");
	  }
      }
    else
      {
	warnOldStyleKindTest();
	if (curToken == QNAME_TOKEN || curToken == OP_MUL
	    || curToken == NCNAME_TOKEN || curToken == NCNAME_COLON_TOKEN)
	  {
	    qname = parseNameTest(defaultElementNamespace);
	    getRawToken();
	  }
	else
	  qname = new Symbol(null);
      }
    return new ElementType(qname);
  }

  private boolean warnedOldStyleKindTest;
  private void warnOldStyleKindTest()
  {
    if (warnedOldStyleKindTest)
      return;
    error('w', "old-style KindTest - first one here");
    warnedOldStyleKindTest = true;
  }

  /** Parse: ["as" SequenceType] */
  public Expression parseOptionalTypeDeclaration ()
      throws java.io.IOException, SyntaxException
  {
    if (! match("as"))
      return null;
    getRawToken();
    return parseDataType();
  }

  public Expression parseDataType()
      throws java.io.IOException, SyntaxException
  {
    Type type = parseItemType();
    int min, max;
    if (type == null)
      return syntaxError("bad syntax - expected DataType");
    if (curToken == '?')
      {
	min = 0;
	max = 1;
      }
    else if (curToken == OP_ADD)
      {
	min = 1;
	max = -1;
      }
    else if (curToken == OP_MUL)
      {
	min = 0;
	max = -1;
      }
    else
      {
	min = 1;
	max = 1;
      }
    if (min != max)
      {
	getRawToken();
	return new QuoteExp(new OccurrenceType(type, min, max));
      }
    return new QuoteExp(type);
  }

  public Type parseItemType()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	if (match("element"))
	  {
	    getRawToken();
	    return parseElementType();
	  }
	if (match("text"))
	  {
	    parseSimpleKindType();
	    return textNodeTest;
	  }
	if (match("node"))
	  {
	    parseSimpleKindType();
	    return anyNodeTest;
	  }
	if (match("empty"))
	  {
	    parseSimpleKindType();
	    return Type.void_type;
	  }
	if (match("item"))
	  {
	    parseSimpleKindType();
	    return Type.pointer_type;
	  }
	String tname = new String(tokenBuffer, 0, tokenBufferLength);
	getRawToken();
	Type type = interpreter.getTypeFor(tname); 
	if (type == null)
	  type = gnu.bytecode.ClassType.make(tname);
	return type;
      }
    else
      return null;
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
	char saveReadState = pushNesting('%');
	getRawToken();
	popNesting(saveReadState);
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

  Symbol parseNameTest(String defaultNamespaceUri)
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
	    return Symbol.make(defaultNamespaceUri, prefix);
	  }
	uri = (String) namespaces.get(prefix);
	if (uri == null)
	  syntaxError("unknown namespace '" + prefix + "'");
	local = null;
      }
    if (uri == null)
      return new Symbol (local == null ? null : local.intern());
    return Symbol.make(uri, local);
  }

  Expression parseNodeTest(int axis)
      throws java.io.IOException, SyntaxException
  {
    Declaration dotDecl = parser.lookup(DOT_VARNAME, -1);
    if (dotDecl == null)
      error("node test when focus is undefined");
    Expression dot = new ReferenceExp(DOT_VARNAME, dotDecl);
    int token = peekOperand();
    /*
    if (token == NCNAME_TOKEN || token == QNAME_TOKEN 
	|| token == NCNAME_COLON_TOKEN || token == OP_MUL)
      {
        Symbol qname = parseNameTest(defaultNamespace; // FIXME null if attribute
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
    if ((axis < 0 || axis == AXIS_CHILD || axis == AXIS_DESCENDANT
	 || axis == AXIS_DESCENDANT_OR_SELF
	 || axis == AXIS_SELF || axis == AXIS_PARENT)
	&&
	(curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN 
	 || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL
	 || curToken == OP_NODE || curToken == OP_TEXT))
      {
	NodePredicate predicate;
	if (curToken == OP_NODE || curToken == OP_TEXT)
	  {
	    if (curToken == OP_NODE)
	      {
		predicate = anyNodeTest;
	      }
	    else // if (curToken == OP_TEXT)
	      {
		predicate = textNodeTest;
	      }
	    if (getRawToken() != ')')
	      return syntaxError("missing '()' after node test");
	  }
	else
	  {
	    Symbol qname = parseNameTest(defaultElementNamespace);
	    predicate = new ElementType(qname);
	  }
	Expression[] args = { dot };
	TreeScanner op;
	if (axis == AXIS_DESCENDANT)
	  op = DescendantAxis.make(predicate);
	else if (axis == AXIS_DESCENDANT_OR_SELF)
	  op = DescendantOrSelfAxis.make(predicate);
	else if (axis == AXIS_SELF)
	  op = SelfAxis.make(predicate);
	else if (axis == AXIS_PARENT)
	  op = ParentAxis.make(predicate);
	else
	  op = ChildAxis.make(predicate);
	exp = new ApplyExp(op, args);
      }
    else if (axis == AXIS_ATTRIBUTE)
      {
	if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN
	    || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL)
	  {
	    Symbol qname = parseNameTest("");
	    Expression[] args = { dot };
	    TreeScanner op = AttributeAxis.make(new AttributeType(qname));
	    exp = new ApplyExp(op, args);
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
	boolean descendants = curToken == SLASHSLASH_TOKEN;

	LambdaExp lexp = new LambdaExp(3);
	Declaration dotDecl = lexp.addDeclaration(DOT_VARNAME);
	dotDecl.setFlag(Declaration.IS_SINGLE_VALUE);
	dotDecl.noteValue (null);  // Does not have a known value.
	lexp.addDeclaration(POSITION_VARNAME, Type.int_type);
	lexp.addDeclaration(LAST_VARNAME, Type.int_type);
	parser.push(lexp);
	if (descendants)
	  {
	    curToken = '/';
	    Expression dot = new ReferenceExp(DOT_VARNAME, dotDecl);
	    Expression[] args = { dot };
	    TreeScanner op = DescendantOrSelfAxis.make(anyNodeTest);
	    lexp.body = new ApplyExp(op, args);
	    //descendants = false; // FIXME Actually simplify below.
	  }
	else
	  {
	    getRawToken();
	    lexp.body = parseStepExpr();
	  }
	parser.pop(lexp);

	/*
	if (lexp.body instanceof ApplyExp)
	  {
	    // Optimize the case of a simple name step.
	    ApplyExp aexp = (ApplyExp) lexp.body;
	    Expression func = aexp.getFunction();
	    Expression[] args = aexp.getArgs();
	    if (false
		&& func == funcNamedChildren && args.length==2
		&& args[0] instanceof ReferenceExp
		&& ((ReferenceExp) args[0]).getBinding() == decl)
	      {
		args[0] = exp;
		if (descendants)
		  func = funcNamedDescendants;
		exp = new ApplyExp (func, args);
		handled = true;
	      }
	    else if (func == funcForwardFilter && args.length==2
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
	*/

	Expression[] args = new Expression[] { exp, lexp };
	Expression func = makeFunctionExp("gnu.xquery.util.RelativeStep",
					  "relativeStep");
	exp = new ApplyExp(func, args);
      }
    return exp;
  }

  Expression parseStepExpr()
      throws java.io.IOException, SyntaxException
  {
    int axis;
    if (curToken == '.' || curToken == DOTDOT_TOKEN)
      {
	axis = curToken == '.' ? AXIS_SELF : AXIS_PARENT;
	getRawToken();
	Declaration dotDecl = parser.lookup(DOT_VARNAME, -1);
	if (dotDecl == null)
	  error("node test when focus is undefined");
	Expression exp = new ReferenceExp(DOT_VARNAME, dotDecl);
	if (axis == AXIS_PARENT)
	  {
	    Expression[] args = { exp };
	    exp = new ApplyExp(ParentAxis.make(anyNodeTest), args);
	  }
	return parseStepQualifiers(exp, axis);
      }
    axis = peekOperand() - OP_AXIS_FIRST;
    if (axis  >= 0 && axis < COUNT_OP_AXIS)
      {
	getRawToken();
	return parseStepQualifiers(parseNodeTest(axis), axis);
      }
    else
      return parseOtherStepExpr();
  }

  Expression parseStepQualifiers(Expression exp, int axis)
    throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
	if (curToken == '[')
	  {
	    int startLine = getLineNumber() + 1;
	    int startColumn = getColumnNumber() + 1;
	    int saveSeenPosition = seenPosition;
	    int saveSawLast = seenLast;
	    getRawToken();
	    LambdaExp lexp = new LambdaExp(3);
	    lexp.setFile(getName());
	    lexp.setLine(startLine, startColumn);
	    Declaration dot = lexp.addDeclaration(DOT_VARNAME);
	    lexp.addDeclaration(POSITION_VARNAME, Type.int_type);
	    lexp.addDeclaration(LAST_VARNAME, Type.int_type);
	    parser.push(lexp);
	    dot.noteValue(null);
	    Expression cond = parseExpr();
	    char kind;
	    Expression valuesFilter;
	    if (axis < 0)
	      {
		kind = 'P';
		valuesFilter = funcExprFilter;
	      }
	    else if (axis == AXIS_ANCESTOR || axis == AXIS_ANCESTOR_OR_SELF
		     || axis == AXIS_PARENT || axis == AXIS_PRECEDING
		     || axis == AXIS_PRECEDING_SIBLING)
	      {
		kind = 'R';
		valuesFilter = funcReverseFilter;
	      }
	    else
	      {
		kind = 'F';
		valuesFilter = funcForwardFilter;
	      }
	    /*)
	    boolean sawPosition = seenPosition > saveSeenPosition;
	    boolean sawLast = seenLast > saveSeenLast;
	    */
	    cond.setFile(getName());
	    cond.setLine(startLine, startColumn);
	    parser.pop(lexp);
	    lexp.body = cond;
	    if (curToken != ']')
	      return syntaxError("missing ']'");
	    getRawToken();
	    Expression[] args = { exp, lexp };
	    exp = new ApplyExp(valuesFilter, args);
	  }
	/*
	else if (curToken == ARROW_TOKEN)
	  ...;
	*/
	else
	  {
	    return exp;
	  }
      }
  }

  /* Parse an OtherStepExpr.
   */
  Expression parseOtherStepExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression e = parsePrimaryExpr();
    e = parseStepQualifiers(e, -1);
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
      return syntaxError("missing expression");
    return exp;
  }

  void parseEntityOrCharRef ()
      throws java.io.IOException, SyntaxException
  {
    int next = read();
    if (next == '#')
      {
	int base;
	next = read();
	if (next == 'x')
	  {
	    base = 16;
	    next = read();
	  }
	else
	  base = 10;
	int value = 0;
	while (next >= 0)
	  {
	    char ch = (char) next;
	    int digit = Character.digit((char) ch, base);
	    if (digit < 0)
	      break;
	    if (value >= 0x8000000)
	      break; // Overflow likely.
	    value = value * base;
	    value += digit;
	    next = read();
	  }
	if (next != ';')
	  {
	    unread();
	    error("invalid character reference");
	  }
	else
	  tokenBufferAppend(value);
      }
    else
      {
	int saveLength = tokenBufferLength;
	while (next >= 0)
	  {
	    char ch = (char) next;
	    if (! isNamePart(ch))
	      break;
	    tokenBufferAppend(ch);
	    next = read();
	  }
	if (next != ';')
	  {
	    unread();
	    error("invalid entity reference");
	    return;
	  }
	String ref = new String(tokenBuffer, saveLength,
				tokenBufferLength - saveLength);
	tokenBufferLength = saveLength;
	appendNamedEntity(ref);
      }
  }

  /** Parse ElementContent (delimiter == '<')  or AttributeContext (otherwise).
   * @param delimiter is '<' if parsing ElementContent, is either '\'' or
   *   '\"' if parsing AttributeContent depending on the starting quote
   * @param result a buffer to place the resulting Expressions.
   */
  void parseContent(char delimiter, Vector result)
      throws java.io.IOException, SyntaxException
  {
    tokenBufferLength = 0;
    boolean preserve = preserveBoundarySpace || delimiter != '<';
    for (;;)
      {
	int next = read();
	if ((next < 0 || next == '{' || next == delimiter)
	    && tokenBufferLength > 0)
	  {
	    if (preserve)
	      {
		String str = new String(tokenBuffer, 0, tokenBufferLength);
		result.addElement(new QuoteExp(str));
	      }
	    tokenBufferLength = 0;
	  }
	if (next < 0)
	  eofError("unexpected end-of-file");
	if (next == '{')
	  {
	    next = read();
	    if (next == '{')
	      {
		tokenBufferAppend('{');
		preserve = true;
	      }
	    else
	      {
		unread(next);
		Expression exp = parseEnclosedExpr();
		if (delimiter != '<')
		  exp = stringValue(exp);
		result.addElement(exp);
		tokenBufferLength = 0;
	      }
	  }
	else if (next == '}')
	  {
	    next = read();
	    if (next == '}')
	      {
		tokenBufferAppend('}');
		preserve = true;
	      }
	    else
	      {
		error("unexpected '}' in element content");
		unread(next);
	      }
	  }
	else if (next == delimiter)
	  {
	    if (delimiter != '<')
	      {
		if (checkNext(delimiter))
		  tokenBufferAppend(delimiter);
		else
		  break;
	      }
	    else
	      {
		if (checkNext('/'))
		  break;
		getRawToken();
		result.addElement(parseElementConstructor());
		tokenBufferLength = 0;
	      }
	  }
	else if (next == '&')
	  {
	    parseEntityOrCharRef();
	    preserve = true;
	  }
	else
	  {
	    if (! preserve)
	      preserve = ! Character.isWhitespace((char) next);
	    tokenBufferAppend((char) next);
	  }
      }
  }

  /** Parse an EnclosedExpr.
   * Assume the '{' has been read.
   */
  Expression parseEnclosedExpr()
      throws java.io.IOException, SyntaxException
  {
    char saveReadState = pushNesting('{');
    peekNonSpace("unexpected end-of-file after '{'");
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() + 1;
    getRawToken();
    Expression exp = parseExpr();
    for (;;)
      {
	if (curToken == '}')
	  break;
	if (curToken == EOF_TOKEN || curToken == ')' || curToken == ']')
	  {
	    exp = syntaxError("missing '}'");
	    break;
	  }
	if (curToken != ',')
	  {
	    exp = syntaxError("missing '}' or ','");
	  }
	getRawToken();
	exp = makeExprSequence(exp, parseExpr());
      }
    exp.setFile(getName());
    exp.setLine(startLine, startColumn);
    popNesting(saveReadState);
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
	Symbol qname = curToken == NCNAME_TOKEN
	  ? Symbol.make(defaultNamespaceUri, name)
	  : parseNameTest(null);
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
    Vector vec = new Vector();
    Expression element = parseNameSpec(defaultElementNamespace, false);
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
	Expression makeAttr = parseNameSpec("", true);
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
	  parseContent((char) ch, vec);
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
    return new ApplyExp(makeFunctionExp("gnu.xquery.util.MakeElement", "makeElement"),
			args);
  }

  Expression parseExprSequence(int rightToken)
      throws java.io.IOException, SyntaxException
  {
    if (curToken == rightToken || curToken == EOF_TOKEN)
      return QuoteExp.voidExp;
    Expression exp = null;
    for (;;)
      {
	Expression exp1 = parseExpr();
	exp = exp == null ? exp1 : makeExprSequence(exp, exp1);
	if (curToken == rightToken || curToken == EOF_TOKEN)
	  break;
	if (nesting == 0 && curToken == EOL_TOKEN)
	  return exp;
	if (curToken != ',')
	  return syntaxError (rightToken == ')' ? "expected ')'"
			       : "confused by syntax error");
	getRawToken();
      }
    return exp;
  }

  Expression parseTypeSwitch()
    throws java.io.IOException, SyntaxException
  {
    char save = pushNesting('t');
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
	pushNesting('c');
	getRawToken();
	Expression caseType = parseDataType();
	popNesting('t');
	LambdaExp lexp = new LambdaExp(1);
	Declaration decl = lexp.addDeclaration(argName,  // FIXME cast
					       (Type) ((QuoteExp) caseType).getValue());

	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'case'");
	parser.push(lexp);
	pushNesting('r');
	Expression caseExpr = parseExpr();
	lexp.body = caseExpr;
	popNesting('t');
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
    popNesting(save);
    Expression[] args = new Expression[vec.size()];
    vec.copyInto(args);
    return new ApplyExp(makeFunctionExp("gnu.kawa.reflect.TypeSwitch",
					"typeSwitch"),
			args);
  }

  char matchConstructorKeyword (int next)
      throws java.io.IOException, SyntaxException
  {
    char kind;
    if (curToken == NCNAME_TOKEN)
      {
	if (match("element"))
	  kind = 'e';
	else if (match("attribute"))
	  kind = 'a';
	else if (match("document") && next == '{')
	  kind = 'd';
	else if (match("text") && next == '{')
	  kind = 't';
	else
	  return '\0';
	if (next != '{' && (kind == 'e' || kind == 'a'))
	  {
	    if (! isNameStart((char) next))
	      return '\0';
	    unread(); // unread 'next' in case it is a single character.
	    mark();
	    getRawToken(); // Skip 'element' or 'attribute'.
	    getRawToken(); // Skip NAME.
	    if (curToken != '{')
	      kind = '\0';
	    reset();
	    read(); // re-read 'next' since caller expects it.
	  }
	return kind;
      }
    return '\0';
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
    char kind;
    if (token == '(')
      {
	getRawToken();
	char saveReadState = pushNesting('(');
	exp = parseExprSequence(')');
	popNesting(saveReadState);
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
	char saveReadState = pushNesting('<');
	exp = parseElementConstructor();
	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
	popNesting(saveReadState);
      }
    else if (token == STRING_TOKEN)
      {
	exp = new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength).intern());
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
	if (decl == null)
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
	int colon = tokenBufferLength;
	while (--colon >= 0 && tokenBuffer[colon] != ':') ;
	Object name;
	if (colon >= 0)
	  {
	    String prefix = new String(tokenBuffer, 0, colon);
	    colon++;
	    String local
	      = new String(tokenBuffer, colon, tokenBufferLength - colon);
	    String uri = (String) namespaces.get(prefix);
	    if (uri == null)
	      {
		try
		  {
		    Class clas = Class.forName(prefix);
		    uri = "class:" + prefix;
		  }
		catch (Exception ex)
		  {
		    syntaxError("unknown namespace '" + prefix + "'");
		    name = new Symbol(local.intern());
		  }
	      }
	    name = Symbol.make(uri, local);
	  }
	else
	  {
	    String str = new String(tokenBuffer, 0, tokenBufferLength);
	    if (str.equals("typeswitch"))
	      return parseTypeSwitch();
	    if (defaultFunctionNamespace == "")
	      name = str.intern(); // kludge
	    else
	      name = Symbol.make(defaultFunctionNamespace, str);
	  }
	startColumn -= tokenBufferLength;
	char save = pushNesting('(');
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

	String varName = null;
	if (name instanceof Symbol)
	  {
	    Symbol sym = (Symbol) name;
	    String lname = sym.getLocalName();
	    if (sym.getNamespaceURI() == XQuery.XQUERY_FUNCTION_NAMESPACE)
	      {
		if (lname == "position")
		  {
		    seenPosition++;
		    varName = POSITION_VARNAME;
		  }
		if (lname == "last")
		  {
		    seenLast++;
		    varName = LAST_VARNAME;
		  }
	      }
	  }
	if (varName != null)
	  {
	    if (args.length != 0)
	      error("arguments in call to " + name);
	    Declaration decl = parser.lookup(varName, -1);
	    if (decl == null)
	      error("undefined context for " + name);
	    exp = new ReferenceExp(varName, decl);
	  }
	else
	  {
	    vec.copyInto(args);
	    ReferenceExp rexp = new ReferenceExp(name, null);
	    rexp.setProcedureName(true);
	    exp = new ApplyExp(rexp, args);
	  }
	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
	popNesting(save);
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
	      return syntaxError("invalid syntax - variable following name");
	    exp = parseFLWRExpression(forOrLet > 0);
	    exp.setFile(getName());
	    exp.setLine(startLine, startColumn - 3);
	    return exp;
	  }
	else if ((kind = matchConstructorKeyword(next)) != '\0')
	  {
	    if (next >= 0)
	      unread();
	    getRawToken();  // Skip 'element'.
	    Vector vec = new Vector();
	    Expression func;
	    if (kind == 'e' || kind == 'a')
	      {
		Expression element
		  = parseNameSpec(defaultElementNamespace, kind == 'a');
		if (element == null)
		  return syntaxError("missing element/attribute name");
		vec.addElement(element);
		if (kind == 'e')
		  func = makeFunctionExp("gnu.xquery.util.MakeElement", "makeElement");
		else
		  func = makeFunctionExp("gnu.xquery.util.MakeAttribute", "makeAttribute");
		getRawToken();
	      }
	    else if (kind == 'd')
	      func = makeFunctionExp("gnu.kawa.xml.DocumentConstructor",
				     "documentConstructor");
	    else /* kind == 't' */
	      func = makeFunctionExp("gnu.kawa.xml.TextConstructor",
				     "textConstructor");
	    char saveReadState = pushNesting('{');
	    peekNonSpace("unexpected end-of-file after '{'");
	    if (curToken != '{')
	      return syntaxError("missing '{'");
	    getRawToken();
	    if (curToken != '}')
	      {
		vec.addElement(parseExpr());
		while (curToken == ',')
		  {
		    getRawToken();
		    vec.addElement(parseExpr());
		  }
	      }
	    popNesting(saveReadState);
	    if (curToken != '}')
	      return syntaxError("missing '}'");
	    Expression[] args = new Expression[vec.size()];
	    vec.copyInto(args);
	    exp = new ApplyExp(func, args);
	    exp.setFile(getName());
	    exp.setLine(startLine, startColumn);
	    getRawToken();
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
    char save = pushNesting('i');
    Expression cond = parseExpr();
    if (curToken != ')')
      return syntaxError("missing ')' after 'if (EXPR'");
    getRawToken();
    if (curToken != NCNAME_TOKEN
	|| tokenBufferLength != 4
	|| ! new String(tokenBuffer, 0, 4).equalsIgnoreCase("then"))
      syntaxError("missing 'then'");
    else
      getRawToken();
    Expression thenPart = parseExpr();
    if (curToken != NCNAME_TOKEN
	|| tokenBufferLength != 4
	|| ! new String(tokenBuffer, 0, 4).equalsIgnoreCase("else"))
      syntaxError("missing 'else'");
    else
      getRawToken();
    popNesting(save);
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
					 String name, char saveNesting)
      throws java.io.IOException, SyntaxException
  {
    ScopeExp sc;
    Expression[] inits = new Expression[1];
    String posVar = null;
    if (isFor)
      {
	boolean sawAt = match("at");
	LambdaExp lexp = new LambdaExp(sawAt ? 2 : 1);
	if (sawAt)
	  {
	    getRawToken();
	    if (curToken == '$')
	      getRawToken();
	    else
	      syntaxError("missing '$'");
	    // FIXME handle QNAME_TOKEN
	    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
	      posVar = new String(tokenBuffer, 0, tokenBufferLength).intern();
	    else
	      return syntaxError("missing Variable");
	    getRawToken();
	  }
	sc = lexp;
	if (match("in"))
	  getRawToken();
	else
	  {	
	    if (curToken == COLON_EQUAL_TOKEN)
	      getRawToken();
	    syntaxError("missing 'in' in 'for' clause");
	  }
      }
    else
      {
	if (curToken == COLON_EQUAL_TOKEN)
	  getRawToken();
	else
	  {	
	    if (match("in"))
	      getRawToken();
	    syntaxError("missing ':=' in 'let' clause");
	  }
	LetExp let = new LetExp(inits);
	sc = let;
      }
    inits[0] = parseBinaryExpr(priority(OP_OR));
    popNesting(saveNesting);
    Declaration decl = sc.addDeclaration(name);
    if (isFor)
      {
	decl.noteValue (null);  // Does not have a known value.
	decl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    if (posVar != null)
      {
	Declaration posDecl = sc.addDeclaration(posVar, Type.int_type);
	posDecl.noteValue(null);
	posDecl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    parser.push(sc);
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
	char save = pushNesting('w');
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
	popNesting(save);
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
    parser.pop(sc);
    if (isFor)
      {
	LambdaExp lexp = (LambdaExp) sc;
	lexp.body = body;
	Expression[] args = { sc, inits[0]};  // SIC
	return new ApplyExp(makeFunctionExp("gnu.kawa.functions.ValuesMap",
					    lexp.min_args == 1 ? "valuesMap"
					    : "valuesMapWithPos"),
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
    char saveNesting = pushNesting(isFor ? 'f' : 'l');
    getRawToken();
    String name;
    if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
      name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    else
      return syntaxError("missing Variable token:"+curToken);
    getRawToken();
    
    return parseFLWRExpression(isFor, name, saveNesting);
  }

  public Expression parseFunctionDefinition(int declLine, int declColumn)
      throws java.io.IOException, SyntaxException
  {
    Object name;
    if (curToken == QNAME_TOKEN)
      {
	int colon = tokenBufferLength;
	while (tokenBuffer[--colon] != ':') ;
	String prefix = new String(tokenBuffer, 0, colon);
	colon++;
	String local = new String(tokenBuffer, colon,
				  tokenBufferLength - colon);
	String uri = (String) namespaces.get(prefix);
	if (uri == null)
	  syntaxError("unknown namespace '" + prefix + "'");
	name = Symbol.make(uri, local);
      }
    else if (curToken == NCNAME_TOKEN)
      {
	String str = new String(tokenBuffer, 0, tokenBufferLength);
	if (defaultFunctionNamespace == "")
	  name = str.intern(); // kludge
	else
	  name = Symbol.make(defaultFunctionNamespace, str);
      }
    else
      return syntaxError("missing function name");
    getRawToken();
    if (curToken != '(')
      return syntaxError("missing parameter list:"+curToken);
    getRawToken();
    LambdaExp lexp = new LambdaExp();
    lexp.setFile(getName());
    lexp.setLine(declLine, declColumn);
    lexp.setName(name);
    Declaration decl = parser.currentScope().addDeclaration(name);
    parser.push(decl);
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
		parser.push(param);
		getRawToken();
		lexp.min_args++;
		lexp.max_args++;
		Expression paramType = parseOptionalTypeDeclaration ();
		if (paramType instanceof QuoteExp)
		  param.setType((Type) ((QuoteExp) paramType).getValue());
		else if (paramType != null)
		  error('w', "parameter type too complex");
	      }
	    else
	      error("missing parameter name");
	    if (curToken == ')')
	      break;
	    if (curToken != ',')
	      return syntaxError("missing ',' in parameter list");
	    getRawToken();
	  }
      }
    getRawToken();
    Expression retType = parseOptionalTypeDeclaration ();
    lexp.body = parseEnclosedExpr();
    parser.pop(lexp);
    if (retType != null)
      Convert.setCoercedReturnValue(lexp, retType, interpreter);
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

  Compilation parser;

  String defaultElementNamespace = "";
  String defaultFunctionNamespace = XQuery.XQUERY_FUNCTION_NAMESPACE;
  Hashtable namespaces = new Hashtable(50);

  void parseSeparator ()
  {
    //System.err.println("parseSep tk:"+curToken);
  }

  /** Parse an expression.
   * Return null on EOF. */
  public Expression parse(Compilation parser)
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
    if (curToken == DEFINE_QNAME_TOKEN)
      {
	int declLine = getLineNumber() + 1;
	int declColumn = getColumnNumber() + 1;
	int next = peekNonSpace("unexpected end-of-file after 'define QName'");
	if (next == '(')
	  {
	    syntaxError("'missing 'function' after 'define'");
	    curToken = NCNAME_TOKEN;
	    return parseFunctionDefinition(declLine, declColumn);
	  }
	else
	  return syntaxError("missing keyword after 'define'");
      }
    if (curToken == DECLARE_FUNCTION_TOKEN)
      {
	int declLine = getLineNumber() + 1;
	int declColumn = getColumnNumber() + 1;
	getRawToken();
	peekNonSpace("unexpected end-of-file after 'define function'");
	char save = pushNesting('d');
	Expression exp = parseFunctionDefinition(declLine, declColumn);
	popNesting(save);
	parseSeparator();
	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
	return exp;
      }
    if (curToken == DECLARE_VARIABLE_TOKEN)
      {
	String name;
	getRawToken();
	if (curToken == '$')
	  getRawToken();
	else
	  syntaxError("missing '$'");
	if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
	  name = new String(tokenBuffer, 0, tokenBufferLength).intern();
	else
	  return syntaxError("missing Variable");
	getRawToken();
	Expression type = parseOptionalTypeDeclaration();
	Declaration decl = parser.currentScope().addDeclaration(name);
	parser.push(decl);
	Expression init = null;
	if (curToken == '{')
	  {
	    init = parseEnclosedExpr();
	    parseSeparator();
	  }
	else if (match("external"))
	  {
	    error("external variables not implemented yet");
	  }
	else
	  {
	    error('e', "expected {expression} or external");
	    if (curToken == OP_EQU || curToken == COLON_EQUAL_TOKEN)
	      getRawToken();
	    // This leave curToken pointing at the token *following* the
	    // expression, but parse is not supposed to read ahead like that.
	    // Luckily, this is only a problem in the error recovery case,
	    // when it is usually harmless.  We could fix it by supressing
	    // the getRawToken in parseMaybePrimaryExpr, which is non-trivial.
	    init = parseMaybePrimaryExpr();
	  }
	SetExp sexp = new SetExp(decl, init);
	sexp.setDefining(true);
	decl.noteValue(init);
	return sexp;
      }
    if (curToken == NCNAME_TOKEN
	&& "namespace".equals((String) curValue))
      {
	if (warnOldVersion)
	  error('w', "use 'declare namespace' instead of 'namespace'");
	curToken = DECLARE_NAMESPACE_TOKEN;
      }
    if (curToken == DECLARE_NAMESPACE_TOKEN)
      {
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next))
	      {
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  return syntaxError("confused after seeing 'declare namespace'");
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
    if (curToken == DEFAULT_ELEMENT_TOKEN
	|| curToken == DEFAULT_FUNCTION_TOKEN
	|| (curToken == NCNAME_TOKEN
	    && "default".equals((String) curValue)))
      {
	int next = nesting == 0 ? skipHSpace() : skipSpace();
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next))
	      {
		boolean forFunctions = curToken == DEFAULT_FUNCTION_TOKEN;
		if (curToken == NCNAME_TOKEN && warnOldVersion)
		  error('w', "use 'default element namespace' instead of 'default namespace'");
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
		if (forFunctions)
		  {
		    error('w', "'declare function namespace' not implemented - ignored");
		  defaultFunctionNamespace = uri.toString();
		  }
		else
		  defaultElementNamespace = uri.toString();
		return QuoteExp.voidExp;
	      }
	  }
      }
    if (curToken == DECLARE_XMLSPACE_TOKEN)
      {
	getRawToken();
	if (curToken != OP_EQU)
	  return syntaxError("missing '=' in xmlspace declaration");
	getRawToken();
	if (match("preserve"))
	  preserveBoundarySpace = true;
	else if (match("skip"))
	  preserveBoundarySpace = false;
	else
	  return syntaxError("xmlspace declaration must be preserve or strip");
	return QuoteExp.voidExp;
      }

    Expression exp = parseExprSequence(EOF_TOKEN);
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
  static final Expression funcNamedDescendantsOrSelf
    = makeFunctionExp("gnu.kawa.xml.NamedDescendants", 
		      "namedDescendantsOrSelf");
  static final Expression funcForwardFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "forwardFilter");
  static final Expression funcReverseFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "reverseFilter");
  static final Expression funcExprFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "exprFilter");

  static final NodeType textNodeTest = new NodeType("text", NodeType.TEXT_OK);
  static final NodeType anyNodeTest = new NodeType("node");

  public void error(String message)
  {
    super.error(message);
  }

  public Expression syntaxError (String message)
    throws java.io.IOException, SyntaxException
  {
    return syntaxError(message, tokenWidth());
  }

  private int tokenWidth()
  {
    switch (curToken)
      {
      case EOF_TOKEN:
	return 0;
      case QNAME_TOKEN:
      case NCNAME_TOKEN:
      case INTEGER_TOKEN:
      case FLOAT_TOKEN:
	return tokenBufferLength;
      default:
	return 1;
      }
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @param columnAdjust number of columns to subtract from current position.
   * @return an ErrorExp
   */
  public Expression syntaxError (String message, int columnAdjust)
    throws java.io.IOException, SyntaxException
  {
    int line = port.getLineNumber();
    int column = port.getColumnNumber();
    error('e', port.getName(), line + 1,
	  column < 0 ? 0 : column + 1 - columnAdjust,
	  message);
    if (interactive)
      {
	curToken = 0;
	curValue = null;
	nesting = 0;
	((InPort) getPort()).readState = '\n';
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

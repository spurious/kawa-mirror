// Copyright (c) 2001, 2002, 2003, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.text.*;
import gnu.expr.*;
import java.util.Vector;
import gnu.kawa.xml.*;
import gnu.xml.NamespaceBinding;
import gnu.bytecode.*;
import gnu.kawa.reflect.OccurrenceType;
import gnu.kawa.functions.Convert;
import gnu.xquery.util.NamedCollator;

/** A class to read xquery forms. */

public class XQParser extends LispReader // should be extends Lexer
{
  int curToken;
  Object curValue;

  /** Value of getLineNumber() at start of current token.
   * Sometimes set otherwise, to report errors. */
  int curLine;

  /** Value of getColumnNumber() at start of current token.
   * Sometimes set otherwise, to report errors. */
  int curColumn;

  XQuery interpreter;

  int seenPosition;
  int seenLast;

  public static boolean warnOldVersion = true;

  /** The internal name of the variable containing '.', the context node. */
  static final Symbol DOT_VARNAME = Symbol.make(null, "$dot$");

  /** The pseduo-function position() is mapped to a reference. */
  static final Symbol POSITION_VARNAME = Symbol.make(null, "$position$");

  /** The pseduo-function last() is mapped to a reference to this variable. */
  static final Symbol LAST_VARNAME = Symbol.make(null, "$last$");

  public static final gnu.kawa.reflect.InstanceOf instanceOf
  = new gnu.kawa.reflect.InstanceOf(XQuery.getInstance(), "instance");

  NameLookup lexical;
  /** Only used in deprecated parseQName. */
  XQResolveNames resolver;

  NamedCollator defaultCollator = null;

  boolean preserveBoundarySpace;

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  /** Skip whitespace.
   * Sets 'index' to the that of the next non-whitespace character,
   * and returns that.  If there are no more non-space characters,
   * returns ' '.  */
  final int skipSpace()
    throws java.io.IOException, SyntaxException
  {
    return skipSpace(true);
  }

  final int skipSpace(boolean verticalToo)
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
	else if (verticalToo
		 ? (ch < 0 || ! Character.isWhitespace((char) ch))
		 : (ch != ' ' && ch != '\t'))
	  return ch;
      }
  }

  final void skipToSemicolon ()
    throws java.io.IOException
  {
    for (;;)
      {
	int next = read();
	if (next < 0 || next == ';')
	  break;
      }
  }

  final void skipOldComment()
    throws java.io.IOException, SyntaxException
  {
    int seenDashes = 0;
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() - 2;
    for (;;)
      {
	int ch = read();
	if (ch == '-')
	  seenDashes++;
	else if (ch == '}' && seenDashes >= 2)
	  return;
	else if (ch < 0)
	  {
	    curLine = startLine;
	    curColumn = startColumn;
	    eofError("non-terminated comment starting here");
	  }
	else
	  seenDashes = 0;
      }
  }

  final void skipComment()
    throws java.io.IOException, SyntaxException
  {
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() - 1;
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
	  {
	    curLine = startLine;
	    curColumn = startColumn;
	    eofError("non-terminated comment starting here");
	  }
	prev = ch;
      }
  }

  /** Do skipSpace followed by unread to find next non-space character. */
  final int peekNonSpace(String message)
    throws java.io.IOException, SyntaxException
  {
    int ch = skipSpace();
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
  static final int COLON_COLON_TOKEN = 'X';

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

  static final int IMPORT_MODULE_TOKEN = 'I'; // <"import" "module">
  static final int IMPORT_SCHEMA_TOKEN = 'T'; // <"import" "schema">
  static final int MODULE_NAMESPACE_TOKEN = 'M'; // <"module" "namespace">
  static final int DECLARE_NAMESPACE_TOKEN = 'N'; // <"declare" "namespace">
  static final int DECLARE_XMLSPACE_TOKEN = 'S'; // <"declare" "xmlspace">
  static final int DEFAULT_ELEMENT_TOKEN = 'E'; // <"default" "element">
  static final int DEFAULT_FUNCTION_TOKEN = 'O'; // <"default" "function">
  static final int DEFAULT_COLLATION_TOKEN = 'G';
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
  static final int OP_INSTANCEOF= OP_BASE + 8 + 2;  // 'instance' 'of'
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
  static final int OP_COMMENT = 233; // 'comment' followed by '('
  static final int OP_PI = 234;   // 'processing-instruction' '('
  static final int OP_DOCUMENT = 235; // 'document-node' '('
  
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

  private int setToken (int token, int width)
  {
    curToken = token;
    curLine = port.getLineNumber() + 1;
    curColumn = port.getColumnNumber() + 1 - width;
    return token;
  }

  int getRawToken()
      throws java.io.IOException, SyntaxException
  {
    int next;
    for (;;)
      {
	next = read();
	if (next < 0)
	  return setToken(EOF_TOKEN, 0);
	if (next == '\n' || next == '\r')
	  {
	    if (nesting <= 0)
	      return setToken(EOL_TOKEN, 0);
	  }
	else if (next == '(')
	  {
	    if (checkNext(':'))
	      skipComment();
	    else
	      return setToken('(', 1);
	  }
	else if (next == '{')
	  {
	    if (! checkNext('-'))
	      return setToken('{', 1);
	    next = read();
	    if (next != '-')
	      {
		// FIXME backup 2 chars. Can fix using special token for '{-'.
		unread();
		unread();
		return setToken('{', 1);
	      }
	    skipOldComment();
	  }
	else if (next != ' ' && next != '\t')
	  break;
      }
    tokenBufferLength = 0;
    curLine = port.getLineNumber() + 1;
    curColumn = port.getColumnNumber();
    char ch = (char) next;
    switch (ch)
      {
      case ')':  case '[':  case ']':  case '}':
      case '$':  case '@':  case ',':  case '?':  case ';':
	break;
      case ':':
	if (checkNext('='))
	  ch = COLON_EQUAL_TOKEN;
	else if (checkNext(':'))
	  ch = COLON_COLON_TOKEN;
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
	      eofError("unexpected end-of-file in string starting here");
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
		tokenBufferAppend((char) next);
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
	  syntaxError("invalid character '"+ch+'\'');
	else
	  syntaxError("invalid character '\\u"+Integer.toHexString(ch)+'\'');
      }
    curToken = ch;
    return ch;
  }

  /** Scan until a given delimiter.
   * On success, text upto the delimiter is in then tokenBuffer (with
   * tokenBufferLength marking its length); the delimiter is not included.
   */
  public void getDelimited(String delimiter)
      throws java.io.IOException, SyntaxException
  {
    tokenBufferLength = 0;
    int dlen = delimiter.length();
    char last = delimiter.charAt(dlen-1);
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  eofError("unexpected end-of-file looking for '"+delimiter+'\'');
	int dstart, j;
	// Look for a match for the last delimiter character.
	if (ch == last
	    && (dstart = tokenBufferLength - (j = dlen - 1)) >= 0)
	  {
	    // Check that the initial part of the delimiter has also been seen.
	    do
	      {
		if (j == 0)
		  {
		    tokenBufferLength = dstart;
		    return;
		  }
		j--;
	      }
	    while (tokenBuffer[dstart+j] == delimiter.charAt(j));
	  }
	tokenBufferAppend((char) ch);
      }
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
      {
	if (nesting == 0)
	  return EOL_TOKEN;
	getRawToken();
      }
    if (curToken == NCNAME_TOKEN)
      {
	int len = tokenBufferLength;
	if (len == 2 || len == 3)
	  {
	    char c1 = tokenBuffer[0];
	    if (len == 2)
	      {
		char c2 = tokenBuffer[1];
		if (c1 == 'o' && c2 == 'r')
		  curToken = OP_OR;
		else if (c1 == 't' && c2 == 'o')
		  curToken = OP_RANGE_TO;
		else if (c1 == 'i' && c2 == 's')
		  curToken = OP_IS;

		// The ValueComp operators 'eq' ... are mapped to the
		// corresponding GeneralComp operators '=' ...
		// So we fail to catch certain errors.  FIXME.
		else if (c1 == 'e' && c2 == 'q')
		  curToken = OP_EQU;
		else if (c1 == 'n' && c2 == 'e')
		  curToken = OP_NEQ;
		else if (c1 == 'g')
		  {
		    if (c2 == 'e')  curToken = OP_GEQ;
		    else if (c2 == 't')  curToken = OP_GRT;
		  }
		else if (c1 == 'l')
		  {
		    if (c2 == 'e')  curToken = OP_LEQ;
		    else if (c2 == 't')  curToken = OP_LSS;
		  }
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
	else if (len == 6)
	  {
	    if (match("except"))
	      curToken = OP_EXCEPT;
	  }
	else if (len == 8)
	  {
	    if (match("instance"))
	      curToken = OP_INSTANCEOF;
	  }
	else if (len == 9)
	  {
	    if (match("intersect"))
	      curToken = OP_INTERSECT;
	  }
	else if (len == 10)
	  {
	    if (match("instanceof")) // obsolete
	      curToken = OP_INSTANCEOF;
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

  int getAxis ()
  {
    // match axis name
    String name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    int i;
    for (i = COUNT_OP_AXIS;  --i >= 0; )
      if (axisNames[i] == name)
	break;
    if (i < 0)
      {
	error("unknown axis name '" + name + '\'');
	i = AXIS_CHILD;
      }
    return (char) (OP_AXIS_FIRST + i);
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
	int next = skipSpace(nesting != 0);
	if (next == '(' && peek() != ':')
	  {
	    int token = FNAME_TOKEN;
	    switch (tokenBuffer[0])
	      {
	      case 'c':
		if (match("comment")) token = OP_COMMENT;
		break;
	      case 'd':
		if (match("document-node")) token = OP_DOCUMENT;
		break;
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
	      case 'p':
		if (match("processing-instruction")) token = OP_PI;
		break;
	      case 't':
		if (match("text")) token = OP_TEXT;
		break;
	      }
	    return curToken = token;
	  }
	if (next == ':' && peek() == ':')
	  return curToken = getAxis();
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	curValue = name;
	switch (next)
	  {
	  case 'd':
	    if (lookingAt("declare", /*"d"+*/ "efault"))
	      {
		getRawToken();
		if (match("function"))
		  return curToken = DEFAULT_FUNCTION_TOKEN;
		if (match("element"))
		  return curToken = DEFAULT_ELEMENT_TOKEN;
		if (match("collation"))
		  return curToken = DEFAULT_COLLATION_TOKEN;
		error("unrecognized/unimplemented 'declare default'");
		skipToSemicolon();
		return peekOperand();
	      }
	  case 'e':
	    if (lookingAt("default", /*"e"+*/ "lement"))
	      {
		if (warnOldVersion)
		  error('w',
			"replace 'default element' by 'declare default element namespace'");
		return curToken = DEFAULT_ELEMENT_TOKEN;
	      }
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
	      {
		if (warnOldVersion)
		  error('w',
			"replace 'default function' by 'declare default function namespace'");
		return curToken = DEFAULT_FUNCTION_TOKEN;
	      }
	    break;
	  case 'm':
	    if (lookingAt("import", /*"m"+*/ "odule"))
	      return curToken = IMPORT_MODULE_TOKEN;
	    break;
	  case 'n':
	    if (lookingAt("declare", /*"n"+*/ "amespace"))
	      return curToken = DECLARE_NAMESPACE_TOKEN;
	    if (lookingAt("default", /*"n"+*/ "amespace"))
	      {
		if (warnOldVersion)
		  error('w',
			"replace 'default namespace' by 'declare default element namespace'");
		return curToken = DEFAULT_ELEMENT_TOKEN;
	      }
	    if (lookingAt("module", /*"n"+*/ "amespace"))
	      return curToken = MODULE_NAMESPACE_TOKEN;
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
	  curToken = getAxis();
	else
	  unread(next);
      }
    return curToken;
  }

  Declaration pushNamespace(String prefix, String uri)
  {
    Declaration decl = makeNamespaceDecl (prefix, uri);
    lexical.push(decl);
    if (prefix == XQuery.DEFAULT_FUNCTION_PREFIX)
      return decl;
    if (prefix == XQuery.DEFAULT_ELEMENT_PREFIX)
      prefix = null;
    else
      prefix = prefix.intern();
    prologNamespaces = new NamespaceBinding(prefix, uri, prologNamespaces);
    return decl;
  }

  private void pushStandardNamespaces ()
  {
    pushNamespace("xml", "http://www.w3.org/XML/1998/namespace");
    pushNamespace("xs", XQuery.SCHEMA_NAMESPACE);
    pushNamespace("xsi", "http://www.w3.org/2001/XMLSchema-instance");
    pushNamespace("fn", XQuery.XQUERY_FUNCTION_NAMESPACE);
    pushNamespace("kawa", XQuery.KAWA_FUNCTION_NAMESPACE);
    pushNamespace("qexo", XQuery.QEXO_FUNCTION_NAMESPACE);
    pushNamespace("local", XQuery.LOCAL_NAMESPACE);
  }

  public XQParser(InPort port, SourceMessages messages, XQuery interp)
  {
    super(port, messages);
    interpreter = interp;
    lexical = new NameLookup(interp);
    nesting = 1;
    pushStandardNamespaces();
  }

  public void setInteractive(boolean v)
  {
    if (interactive != v)
      if (v) nesting--; else nesting++;
    interactive = v;
  }
  
  protected ReadTable getReadTable () { return xqlReadTable; }

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
	func = new QuoteExp(new PrimProcedure(ClassType.make("gnu.math.IntNum").getDeclaredMethod("remainder", 2)));
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
      case OP_INTERSECT:
	func = makeFunctionExp("gnu.kawa.xml.IntersectNodes",
			       "intersectNodes");
	break;
      case OP_EXCEPT:
	func = makeFunctionExp("gnu.kawa.xml.IntersectNodes", "exceptNodes");
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
	    qname = parseQName(defaultElementNamespace);
	    getRawToken();
	    if (curToken == ',')
	      {
		getRawToken();
		Symbol tname = parseQName(defaultElementNamespace);
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
	    || curToken == NCNAME_TOKEN)
	  {
	    qname = parseQName(defaultElementNamespace);
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

  public void setType (Declaration decl, Expression type)
  {
    if (type instanceof QuoteExp)
      decl.setType((Type) ((QuoteExp) type).getValue());
    else if (type != null)
      error('w', "type is too complex");
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
	if (match("comment"))
	  {
	    parseSimpleKindType();
	    return commentNodeTest;
	  }
	if (match("document-node"))
	  {
	    parseSimpleKindType();
	    return documentNodeTest;
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
	if (match("processing-instruction"))
	  {
	    parseSimpleKindType();
	    // FIXME don't support processing-instruction(target)
	    return piNodeTest;
	  }
	String tname = new String(tokenBuffer, 0, tokenBufferLength);
	getRawToken();
	Type type = interpreter.getTypeFor(tname); 
	if (type == null)
	  type = ClassType.make(tname);
	return type;
      }
    else
      return null;
  }

  Expression parseExpr()
      throws java.io.IOException, SyntaxException
  {
    return parseSortExpr();
  }

  final Expression parseExprSingle ()
      throws java.io.IOException, SyntaxException
  {
    return parseBinaryExpr(priority(OP_OR));
  }

  Expression parseBinaryExpr(int prio)
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseUnaryExpr();
    for (;;)
      {
	int token = peekOperator();
	if (token == EOL_TOKEN
	    // Following makes for better error handling.
	    || (token == OP_LSS && peek() == '/'))
	  return exp;  
	int tokPriority = priority(token);
	if (tokPriority < prio || tokPriority > (OP_MOD >> 2))
	  return exp;
	char saveReadState = pushNesting('%');
	boolean sawInstanceof = false;
	if (token == OP_INSTANCEOF)
	  {
	    curToken = NCNAME_TOKEN;
	    sawInstanceof = match("instanceof");
	  }
	getRawToken();
	popNesting(saveReadState);
	if (token == OP_INSTANCEOF)
	  {
	    if (sawInstanceof)
	      {
		if (warnOldVersion)
		  error('w', "use 'instanceof of' (two words) instead of 'instanceof'");
	      }
	    else
	      {
		if (match("of"))
		  getRawToken();
		else
		  error('e', "expected 'instance' to be followed by 'of'");
	      }
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
      int op = curToken;
      getRawToken();
      exp = parseUnionExpr();
      exp = makeBinary(op, new QuoteExp(gnu.math.IntNum.zero()), exp);
    }
    else
      exp = parseUnionExpr();
    return exp;
  }

  Expression parseUnionExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseIntersectExceptExpr();
    for (;;)
      {
	int op = peekOperator();
	if (op != OP_UNION)
	  break;
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
    for (;;)
      {
	int op = peekOperator();
	if (op != OP_INTERSECT && op != OP_EXCEPT)
	  break;
	getRawToken();
	Expression exp2 = parsePathExpr();
	exp = makeBinary(op, exp, exp2);
      }
    return exp;
  }

  Expression parsePathExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression step1;
    if (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	Declaration dotDecl = comp.lookup(DOT_VARNAME, -1);
	if (dotDecl == null)
	  error("node test when focus is undefined");
	Expression dot = new ReferenceExp(DOT_VARNAME, dotDecl);
	step1 = new ApplyExp(ClassType.make("gnu.kawa.xml.Nodes")
			     .getDeclaredMethod("root", 1),
			     new Expression[] { dot } );
      }
    else
      step1 = parseStepExpr();
    return parseRelativePathExpr(step1);
  }

  /** Returns two expressions, one that evaluated to a namespace, and
   * one that evaluates to a local name.  These will normally constant
   * folded to a Symbol, but we cannot do that yet. */
  Expression[] parseNameTest (boolean attribute)
      throws java.io.IOException, SyntaxException
  {
    Expression[] name = new Expression[2];
    String local = null, prefix = null, uri = null;
    if (curToken == QNAME_TOKEN)
      {
	int colon = tokenBufferLength;
	while (tokenBuffer[--colon] != ':') ;
	prefix = new String(tokenBuffer, 0, colon);
	colon++;
	local = new String(tokenBuffer, colon,
			   tokenBufferLength - colon);
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
	uri = attribute ? "" : defaultElementNamespace;
      }
    else if (curToken == NCNAME_COLON_TOKEN)
      {
	prefix = new String(tokenBuffer, 0, tokenBufferLength);
	int next = read();
	if (next != '*')
	  syntaxError("invalid characters after 'NCName:'");
	local = null;
      }
    if (uri != null)
      name[0] = new QuoteExp(uri);
    else if (prefix == null)
      name[0] = QuoteExp.nullExp;
    else
      name[0]
	= new ReferenceExp((Interpreter.NAMESPACE_PREFIX + prefix).intern());
    name[1] = new QuoteExp(local == null ? null : local.intern());
    return name;
  }

  /** This is deprecated because it only sees namespace prefix in the
   * module prefix.  It doesn't see prefixes in namespace attributes.
   * The correct solution is to defer anmespace resolution to "resolve" time,
   * as parseNameTest does. */
  Symbol parseQName (String defaultNamespaceUri)
      throws java.io.IOException, SyntaxException
  {
    String local = null, uri = null;
    if (curToken == QNAME_TOKEN)
      {
	return resolver.namespaceResolve(new String(tokenBuffer, 0, tokenBufferLength), false);
      }
    else if (curToken == OP_MUL)
      {
      }
    else if (curToken == NCNAME_TOKEN)
      {
	local = new String(tokenBuffer, 0, tokenBufferLength);
	uri = defaultNamespaceUri;
      }
    if (uri == null)
      return new Symbol (local == null ? null : local.intern());
    return Symbol.make(uri, local);
  }

  Expression parseNodeTest(int axis)
      throws java.io.IOException, SyntaxException
  {
    Declaration dotDecl = comp.lookup(DOT_VARNAME, -1);
    if (dotDecl == null)
      error("node test when focus is undefined");
    int token = peekOperand();

    if (curToken == '@' && axis < 0)
      {
	getRawToken();
	axis = AXIS_ATTRIBUTE;
      }
    
    Expression exp;
    Expression[] args = new Expression[1];
    if (curToken == OP_NODE || curToken == OP_TEXT)
      {	
	NodePredicate predicate;
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
	args[0] = new QuoteExp(predicate);
      }
    else if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN 
	     || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL)
      {
	Expression[] name = new Expression[2];
	ClassType nodeType = ClassType.make(axis == AXIS_ATTRIBUTE
					    ? "gnu.kawa.xml.AttributeType"
					    : "gnu.kawa.xml.ElementType");
	ApplyExp elt = new ApplyExp(nodeType.getDeclaredMethod("make", 2),
				    parseNameTest(axis == AXIS_ATTRIBUTE));
	elt.setFlag(ApplyExp.INLINE_IF_CONSTANT);
	args[0] = elt;
      }
    else if (axis >= 0)
      return syntaxError("unsupported axis '"+axisNames[axis]+"::'");
    else
      return null;

    String axisName;
    switch (axis)
      {
      default: /*case AXIS_CHILD: case -1: */ axisName = "Child";  break;
      case AXIS_DESCENDANT:         axisName = "Descendant";       break;
      case AXIS_DESCENDANT_OR_SELF: axisName = "DescendantOrSelf"; break;
      case AXIS_SELF:               axisName = "Self";             break;
      case AXIS_PARENT:             axisName = "Parent";           break;
      case AXIS_ANCESTOR:           axisName = "Ancestor";         break;
      case AXIS_ANCESTOR_OR_SELF:   axisName = "AncestorOrSelf";   break;
      case AXIS_FOLLOWING:          axisName = "Following";        break;
      case AXIS_FOLLOWING_SIBLING:  axisName = "FollowingSibling"; break;
      case AXIS_PRECEDING:          axisName = "Preceding";        break;
      case AXIS_PRECEDING_SIBLING:  axisName = "PrecedingSibling"; break;
      case AXIS_ATTRIBUTE:          axisName = "Attribute";        break;
      }
    ClassType axisClass = ClassType.make("gnu.kawa.xml."+axisName+"Axis");
    ApplyExp mkAxis = new ApplyExp(axisClass.getDeclaredMethod("make", 1),
				   args);
    mkAxis.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    Expression[] dotArg = { new ReferenceExp(DOT_VARNAME, dotDecl) };
    exp = new ApplyExp(mkAxis, dotArg);
    getRawToken();
    return exp;
  }

  Expression parseRelativePathExpr(Expression exp)
      throws java.io.IOException, SyntaxException
  {
    while (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	boolean descendants = curToken == SLASHSLASH_TOKEN;

	LambdaExp lexp = new LambdaExp(3);
	Declaration dotDecl = lexp.addDeclaration(DOT_VARNAME);
	dotDecl.setFlag(Declaration.IS_SINGLE_VALUE);
	dotDecl.noteValue (null);  // Does not have a known value.
	lexp.addDeclaration(POSITION_VARNAME, LangPrimType.intType);
	lexp.addDeclaration(LAST_VARNAME, LangPrimType.intType);
	comp.push(lexp);
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
	comp.pop(lexp);

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
	Declaration dotDecl = comp.lookup(DOT_VARNAME, -1);
	if (dotDecl == null)
	  error("node test3 when focus is undefined");
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
	    comp.push(lexp);
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
	    comp.pop(lexp);
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
      {
	exp = syntaxError("missing expression");
	if (curToken != EOF_TOKEN)
	  getRawToken();
	return exp;
      }
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

  /** Parse ElementContent (delimiter == '<')  or AttributeContent (otherwise).
   * @param delimiter is '<' if parsing ElementContent, is either '\'' or
   *   '\"' if parsing AttributeContent depending on the starting quote
   * @param result a buffer to place the resulting Expressions.
   */
  void parseContent(char delimiter, Vector result)
      throws java.io.IOException, SyntaxException
  {
    tokenBufferLength = 0;
    int prevEnclosed = result.size() - 1;
    boolean skipBoundarySpace = ! preserveBoundarySpace && delimiter == '<';
    boolean skippable = skipBoundarySpace;
    Expression makeText = null;
    for (;;)
      {
	int next = read();
	if (next == delimiter && delimiter != '<' && checkNext(delimiter))
	  {
	    tokenBufferAppend(delimiter);
	    continue;
	  }
	if (next == delimiter || next < 0 || next == '{')
	  {
	    if (tokenBufferLength > 0 && ! skippable)
	      {
		Expression str
		  = new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength));
		result.addElement(str);
	      }
	    else if (next == '{' && prevEnclosed == result.size())
	      {
		
		Expression[] args = { new QuoteExp("") };
		if (makeText == null)
		  makeText = makeFunctionExp("gnu.kawa.xml.MakeText",
					     "makeText");
		result.addElement(new ApplyExp(makeText, args));
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
		skippable = false;
	      }
	    else
	      {
		unread(next);
		Expression exp = parseEnclosedExpr();
		if (delimiter != '<')
		  exp = stringValue(exp); // FIXME
		result.addElement(exp);
		tokenBufferLength = 0;
		prevEnclosed = result.size();
		skippable = skipBoundarySpace;
	      }
	  }
	else if (next == '}')
	  {
	    next = read();
	    if (next == '}')
	      {
		tokenBufferAppend('}');
		skippable = false;
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
		break;
	      }
	    else
	      {
		next = read();
		if (next == '/')
		  break;
		result.addElement(parseXMLConstructor(next));
		tokenBufferLength = 0;
		skippable = skipBoundarySpace;
	      }
	  }
	else if (next == '&')
	  {
	    parseEntityOrCharRef();
	    skippable = false;
	  }
	else
	  {
	    if (skippable)
	      skippable = Character.isWhitespace((char) next);
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
	  exp = syntaxError("missing '}' or ','");
	else
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
	return new QuoteExp(name.intern());
      }
    else if (curToken == '{')
      {
	return parseEnclosedExpr();
      }
    else
      return null;
  }

  Declaration makeNamespaceDecl (String prefix, String uri)
  {
    String sym = (Interpreter.NAMESPACE_PREFIX + prefix).intern();
    Declaration decl = new Declaration(sym);
    decl.setType(gnu.bytecode.Type.tostring_type);
    decl.setFlag(Declaration.IS_CONSTANT|Declaration.IS_NAMESPACE_PREFIX);
    decl.setPrivate(true);
    decl.noteValue(new QuoteExp(uri));
    return decl;
  }

  /** Parse an ElementConstructor or other constructs starting with '<'.
   * Assume initial '<' has been processed.
   * @param next next character (after '<').
   */
  Expression parseXMLConstructor(int next)
      throws java.io.IOException, SyntaxException
  {
    Expression exp;
    if (next == '!')
      {
	next = read();
	if (next == '-' && peek() == '-')
	  {
	    skip();
	    getDelimited("-->");
	    Expression[] args =
	      { new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength)) };
	    exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.CommentConstructor",
					       "commentConstructor"),
			       args);
	  }
	else if (next == '[' && read() == 'C' && read() == 'D'
		 && read() == 'A' && read() == 'T' && read() == 'A'
		 && read() == '[')
	  {
	    getDelimited("]]>");
	    Expression[] args =
	      { new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength)) };
	    exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.MakeCDATA",
					       "makeCDATA"),
			       args);
	  }
	else
	  exp = syntaxError("'<!' must be followed by '--' or '[CDATA['");
      }
    else if (next == '?')
      {
	next = peek();
	if (next < 0 || ! isNameStart((char) next)
	    || getRawToken() != NCNAME_TOKEN)
	  syntaxError("missing target after '<?'");
	String target = new String(tokenBuffer, 0, tokenBufferLength);
	skipSpace();
	unread();
	getDelimited("?>");
	String content = new String(tokenBuffer, 0, tokenBufferLength);
	Expression[] args = { new QuoteExp(target), new QuoteExp(content) };
	exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.MakeProcInst",
					   "makeProcInst"),
			   args);
      }
    else
      {
	unread(next);
	getRawToken();
	char saveReadState = pushNesting('<');
	exp = parseElementConstructor();
	popNesting(saveReadState);
      }
    return exp;
  }

  /** Generate code to cast argument to a QName
   * (which is implemented using <code>SName</code>). */
  Expression castQName (Expression value)
  {
    return new ApplyExp(new ReferenceExp(XQResolveNames.xsQNameDecl),
			new Expression[] { value });
  }

  /** Parse ElementConstructor.
   * Assume initial '<' has been processed,
   * and we're looking at the next token..
   * Reads through end of the end tag.  FIXME
   */
  Expression parseElementConstructor()
      throws java.io.IOException, SyntaxException
  {
    // Note that we cannot do namespace resolution at parse time,
    // because of constructs like this:  <a x="{$x:v}" xmlns:x="xx"/>
    // Instead we defer namespaced lookup until XQResolveNames.  (Mostly -
    // some places still incorrectly do premature namespace resolution.)
    if (curToken != NCNAME_TOKEN && curToken != QNAME_TOKEN)
      return syntaxError("expected QName after '<'");
    String startTag = new String(tokenBuffer, 0, tokenBufferLength);
    Vector vec = new Vector();
    Expression[] args;
    vec.addElement(castQName(new QuoteExp(startTag)));
    NamespaceBinding namespaceOuter = namespaceBindings;
    int ch;
    for (;;)
      {
	ch = skipSpace();
	if (ch < 0 || ch == '>' || ch == '/')
	  break;
	unread(ch);
	getRawToken();
	int vecSize = vec.size();
	if (curToken != NCNAME_TOKEN && curToken != QNAME_TOKEN)
	  break;
	String attrName = new String(tokenBuffer, 0, tokenBufferLength);
	int startLine = getLineNumber() + 1;
	int startColumn = getColumnNumber() + 1 - tokenBufferLength;
	String defininingNamespace = null;
	if (curToken == NCNAME_TOKEN)
	  {
	    if (attrName.equals("xmlns"))
	      defininingNamespace = "";
	  }
	else
	  {
	    if (attrName.startsWith("xmlns:"))
	      defininingNamespace = attrName.substring(6).intern();
	  }
	Expression makeAttr
	  = defininingNamespace != null ? null
	  : MakeAttribute.makeAttributeExp;
	vec.addElement(castQName(new QuoteExp(attrName)));
	ch = skipSpace();
	if (ch != '=')
	  return syntaxError("missing '=' after attribute");
	ch = skipSpace();
	if (ch == '{')
	  {
	    if (warnOldVersion)
	      error('w', "enclosed attribute value expression should be quoted");
	    vec.addElement(stringValue(parseEnclosedExpr()));
	  }
	else
	  parseContent((char) ch, vec);
	int n = vec.size() - vecSize;
	if (defininingNamespace != null)
	  {
	    String ns = "";
	    if (n == 1)
	      ns = "";
	    else if (n > 2 || ! (vec.elementAt(vecSize+1) instanceof QuoteExp))
	      syntaxError("enclosed expression not allowed in namespace declaration");
	    else
	      ns = ((QuoteExp) vec.elementAt(vecSize+1)).getValue()
		.toString().intern();
	    vec.setSize(vecSize);
	    if (defininingNamespace == "")
	      defininingNamespace = null;
	    namespaceBindings
	      = new NamespaceBinding(defininingNamespace,
				     ns == "" ? null : ns,
				     namespaceBindings);
	  }
	else
	  {
	    args = new Expression[n];
	    for (int i = n;  --i >= 0; )
	      args[i] = (Expression) vec.elementAt(vecSize + i);
	    vec.setSize(vecSize);
	    ApplyExp aexp = new ApplyExp(makeAttr, args);
	    aexp.setFile(getName());
	    aexp.setLine(startLine, startColumn);
	    vec.addElement(aexp);
	  }
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
	    String tag = new String(tokenBuffer, 0, tokenBufferLength);
	    if (! (tag.equals(startTag)))
	      return syntaxError("'<"+startTag+">' closed by '</"+tag+">'");
	    ch = skipSpace();
	  }
	if (ch != '>')
	  return syntaxError("missing '>' after end element");
      }
    args = new Expression[vec.size()];
    vec.copyInto(args);
    MakeElement mkElement = new MakeElement();
    Expression result = new ApplyExp(new QuoteExp(mkElement), args);

    // Reverse current set of namespace bindings to match input order.
   NamespaceBinding nsBindings
     = namespaceBindings.reversePrefix(namespaceOuter);
    if (namespaceOuter != nsBindings)
      {
	int count = nsBindings.count(namespaceOuter);
	Expression [] inits = new Expression[count];
	LetExp let = new LetExp(inits);
	int i = 0;
	for (NamespaceBinding ns = nsBindings;
	     ns != namespaceOuter;  ns = ns.getNext())
	  {
	    Declaration decl = makeNamespaceDecl(ns.getPrefix(), ns.getUri());
	    let.addDeclaration(decl);
	    inits[i++] = decl.getValue();
	  }

	let.setBody(result);
	result = let;
	namespaceBindings = namespaceOuter;
      }
    if (nsBindings != NamespaceBinding.predefinedXML)
      mkElement.setNamespaceNodes(nsBindings);
    return result;
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
    Declaration decl;
    Vector vec = new Vector();
    vec.addElement(selector);
    while (match("case"))
      {
	pushNesting('c');
	getRawToken();
	if (curToken == '$')
	  {
	    decl = parseVariableDeclaration();
	    if (decl == null)
	      return syntaxError("missing Variable after '$'");
	    getRawToken();
	    if (match("as"))
	      getRawToken();
	    else
	      error('e', "missing 'as'");
	  }
	else
	  decl = new Declaration("(arg)");
	decl.setType((Type) ((QuoteExp) parseDataType()).getValue());
	popNesting('t');
	LambdaExp lexp = new LambdaExp(1);
	lexp.addDeclaration(decl);
	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'case'");
	comp.push(lexp);
	pushNesting('r');
	Expression caseExpr = parseExpr();
	lexp.body = caseExpr;
	popNesting('t');
	comp.pop(lexp);
	vec.addElement(lexp);
      }
    if (curToken == '$')
      {
	decl = parseVariableDeclaration();
	if (decl == null)
	  return syntaxError("missing Variable after '$'");
	getRawToken();
      }
    else
      decl = new Declaration("(arg)");
    LambdaExp lexp = new LambdaExp(0);
    lexp.addDeclaration(decl);
    if (match("default"))
      {
	getRawToken();
	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'default'");
	comp.push(lexp);
	Expression defaultExpr = parseExpr();
	lexp.body = defaultExpr;
	comp.pop(lexp);
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
	else if (match("comment") && next == '{')
	  kind = 'c';
	/*
	else if (match("processing-instruction") && next == '{')
	  kind = 'p';
	*/
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
    int startLine = curLine;
    int startColumn = curColumn;
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
    else if (token == '{')
      {
	exp = syntaxError("saw unexpected '{' - assume you meant '('");
	parseEnclosedExpr();
      }
    else if (token == OP_LSS)
      {
	int next = read();
	if (next == '/')
	  {
	    getRawToken();
	    String msg;
	    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN
		|| curToken == NCNAME_COLON_TOKEN)
	      msg = "saw end tag '</" + new String(tokenBuffer, 0, tokenBufferLength) + ">' not in an element constructor";
	    else
	      msg = "saw end tag '</' not in an element constructor";
	    curLine = startLine;
	    curColumn = startColumn;
	    exp = syntaxError(msg);
	    while (curToken != OP_GRT && curToken != EOF_TOKEN && curToken != EOL_TOKEN)
	      getRawToken();
	    return exp;
	  }
	else 
	  {
	    exp = parseXMLConstructor(next);
	    exp.setFile(getName());
	    exp.setLine(startLine, startColumn);
	  }
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
	Object name = parseVariable();
	if (name == null)
	  return syntaxError("missing Variable");
	exp = new ReferenceExp(name);
      }
    else if (token == FNAME_TOKEN)
      {
	/*
	if (colon >= 0)
	  {
	    String local, uri;
	    String prefix = new String(tokenBuffer, 0, colon);
	    colon++;
	    local = new String(tokenBuffer, colon, tokenBufferLength - colon);
	    uri = (String) lookupNamespace(prefix);
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
		    uri = defaultFunctionNamespace;
		  }
	      }
	    name = Symbol.make(uri, local);
	  }
	else
	*/
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	if (name.equals("typeswitch"))
	  return parseTypeSwitch();
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

	vec.copyInto(args);
	ReferenceExp rexp = new ReferenceExp(name, null);
	rexp.setProcedureName(true);
	exp = new ApplyExp(rexp, args);

	exp.setFile(getName());
	exp.setLine(startLine, startColumn);
	popNesting(save);
      }
    else if (token == NCNAME_TOKEN || token == QNAME_TOKEN)
      {
	int next = skipSpace(nesting != 0);
	if (next == '$')
	  {
	    // A FLWR-expression isn't technically a PrimaryExpr.
	    // Does it matter?  FIXME.
	    if (match("let"))
	      exp = parseFLWRExpression(false);
	    else if (match("for"))
	      exp = parseFLWRExpression(true);
	    else if (match("some"))
	      exp = parseQuantifiedExpr(false);
	    else if (match("every"))
	      exp = parseQuantifiedExpr(true);
	    else
	      return syntaxError("invalid syntax - variable following name");
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
		// FIXME - rethink this after next spec revision, which
		// will hopefully clarify namespace management here.
		Expression element
		  = parseNameSpec(defaultElementNamespace, kind == 'a');
		if (element == null)
		  return syntaxError("missing element/attribute name");
		vec.addElement(castQName(element));
		if (kind == 'e')
		  {
		    MakeElement mk = new MakeElement();
		    if (namespaceBindings != NamespaceBinding.predefinedXML)
		      mk.setNamespaceNodes(namespaceBindings);
		    func = new QuoteExp(mk);
		  }
		else
		  func = MakeAttribute.makeAttributeExp;
		getRawToken();
	      }
	    else if (kind == 'd')
	      func = makeFunctionExp("gnu.kawa.xml.DocumentConstructor",
				     "documentConstructor");
	    else if (kind == 'c')
	      func = makeFunctionExp("gnu.kawa.xml.CommentConstructor",
				     "commentConstructor");
	    else /* kind == 't' */
	      func = makeFunctionExp("gnu.kawa.xml.MakeText",
				     "makeText");
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
	int ch = skipSpace(false);
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
    if (! match("then"))
      syntaxError("missing 'then'");
    else
      getRawToken();
    Expression thenPart = parseExpr();
    if (! match("else"))
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

  /** Parse a Variable. */
  public Object parseVariable ()
      throws java.io.IOException, SyntaxException
  {
    if (curToken == '$')
      getRawToken();
    else
      syntaxError("missing '$' before variable name");
    String str = new String(tokenBuffer, 0, tokenBufferLength);
    // Note we cannot do namespace resolution here - see comment in
    // parseElementConstructor.
    if (curToken == QNAME_TOKEN)
      return str;
    else if (curToken == NCNAME_TOKEN)
      return Symbol.make("", str);
    else
      return null;
  }

  public Declaration parseVariableDeclaration ()
      throws java.io.IOException, SyntaxException
  {
    Object name = parseVariable();
    if (name == null)
      return null;
    Declaration decl = new Declaration(name);
    decl.setFile(getName());
    decl.setLine(getLineNumber() + 1,
		 getColumnNumber() + 1 - tokenBufferLength);
    return decl;
  }

  /** Parse a let- or a for-expression.
   * Assume the 'let'/'for'-token has been seen, and we've read '$'. */
  public Expression parseFLWRExpression (boolean isFor)
      throws java.io.IOException, SyntaxException
  {
    char saveNesting = pushNesting(isFor ? 'f' : 'l');
    curToken = '$';
    Declaration decl = parseVariableDeclaration();
    if (decl == null)
      return syntaxError("missing Variable - saw "+tokenString());
    getRawToken();

    Expression type = parseOptionalTypeDeclaration();
    ScopeExp sc;
    Expression[] inits = new Expression[1];
    Declaration posDecl = null;
    if (isFor)
      {
	boolean sawAt = match("at");
	LambdaExp lexp = new LambdaExp(sawAt ? 2 : 1);
	if (sawAt)
	  {
	    getRawToken();
	    if (curToken == '$')
	      {
		posDecl = parseVariableDeclaration();
		getRawToken();
	      }
	    if (posDecl == null)
	      syntaxError("missing Variable after 'at'");
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
    inits[0] = parseExprSingle();
    popNesting(saveNesting);
    comp.push(sc);
    sc.addDeclaration(decl);
    setType(decl, type);
    if (isFor)
      {
	decl.noteValue (null);  // Does not have a known value.
	decl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    if (posDecl != null)
      {
	sc.addDeclaration(posDecl);
	posDecl.setType(LangPrimType.intType);
	posDecl.noteValue(null);
	posDecl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    Expression body;
    if (curToken == ',')
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after ','");
	body = parseFLWRExpression(isFor);
      }
    else if (match("for"))
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after 'for'");
	body = parseFLWRExpression(true);
      }
    else if (match("let"))
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after 'let'");
	body = parseFLWRExpression(false);
      }
    else
      {
	Expression cond;
	char save = pushNesting('w');
	if (curToken == OP_WHERE)
	  {
	    getRawToken();
	    cond = parseExprSingle();
	  }
	else if (match("where"))
	  {
	    cond = parseExprSingle();
	  }
	else
	  cond = null;
	popNesting(save);
	boolean sawStable = match("stable");
	if (sawStable)
	  getRawToken();
	if (match ("order"))
	  return syntaxError("'order by' clause not implemented yet");
	boolean sawReturn = match("return");
	if (! sawReturn && ! match("let") && ! match("for"))
	  return syntaxError("missing 'return' clause");
	peekNonSpace("unexpected eof-of-file after 'return'");
	int bodyLine = getLineNumber() + 1;
	int bodyColumn = getColumnNumber() + 1;
	if (sawReturn)
	  getRawToken();
	body = parseExprSingle();
	if (cond != null)
	  {
	    body = new IfExp(booleanValue(cond), body, QuoteExp.voidExp);
	  }
	body.setFile(getName());
	body.setLine(bodyLine, bodyColumn);
      }
    comp.pop(sc);
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

  /** Parse a some- or an every-expression.
   * Assume the 'some'/'every'-token has been seen, and we've read '$'. */
  public Expression parseQuantifiedExpr (boolean isEvery)
      throws java.io.IOException, SyntaxException
  {
    char saveNesting = pushNesting(isEvery ? 'e' : 's');
    curToken = '$';
    Declaration decl = parseVariableDeclaration();
    if (decl == null)
      return syntaxError("missing Variable token:"+curToken);
    getRawToken();
    
    LambdaExp lexp = new LambdaExp(1);
    lexp.addDeclaration(decl);
    decl.noteValue (null);  // Does not have a known value.
    decl.setFlag(Declaration.IS_SINGLE_VALUE);
    setType(decl, parseOptionalTypeDeclaration());

    if (match("in"))
      getRawToken();
    else
      {	
	if (curToken == COLON_EQUAL_TOKEN)
	  getRawToken();
	syntaxError("missing 'in' in QuantifiedExpr");
      }
    Expression[] inits = { parseExprSingle() };
    popNesting(saveNesting);
    comp.push(lexp);
    Expression body;
    if (curToken == ',')
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after ','");
	body = parseQuantifiedExpr(isEvery);
      }
    else
      {
	boolean sawSatisfies = match("satisfies");
	if (! sawSatisfies && ! match("every") && ! match("some"))
	  return syntaxError("missing 'satisfies' clause");
	peekNonSpace("unexpected eof-of-file after 'satisfies'");
	int bodyLine = getLineNumber() + 1;
	int bodyColumn = getColumnNumber() + 1;
	if (sawSatisfies)
	  getRawToken();
	body = parseExprSingle();
	body.setFile(getName());
	body.setLine(bodyLine, bodyColumn);
      }
    comp.pop(lexp);
    lexp.body = body;
    Expression[] args = { lexp, inits[0]};  // SIC
    return new ApplyExp(makeFunctionExp("gnu.xquery.util.ValuesEvery",
					isEvery ? "every" : "some"),
			args);
  }

  public Expression parseFunctionDefinition(int declLine, int declColumn)
      throws java.io.IOException, SyntaxException
  {
    String local, uri;
    if (curToken != QNAME_TOKEN && curToken != NCNAME_TOKEN)
      return syntaxError("missing function name");
    String name = new String(tokenBuffer, 0, tokenBufferLength);
    if (name.indexOf(':') < 0)
      error('w', "defined function must have qualified name");
    getRawToken();
    if (curToken != '(')
      return syntaxError("missing parameter list:"+curToken);
    getRawToken();
    LambdaExp lexp = new LambdaExp();
    lexp.setFile(getName());
    lexp.setLine(declLine, declColumn);
    lexp.setName(name);
    Declaration decl = comp.currentScope().addDeclaration(name);
    if (comp.isStatic())
      decl.setFlag(Declaration.STATIC_SPECIFIED);
    comp.push(decl);
    decl.setCanRead(true);
    decl.setProcedureDecl(true);
    decl.setFile(getName());
    decl.setLine(declLine, declColumn);
    comp.push(lexp);
    if (curToken != ')')
      {
	for (;;)
	  {
	    Declaration param = parseVariableDeclaration();
	    if (param == null)
	      error("missing parameter name");
	    else
	      {
		lexp.addDeclaration(param);
		getRawToken();
		lexp.min_args++;
		lexp.max_args++;
		setType(param, parseOptionalTypeDeclaration());
	      }
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
    comp.pop(lexp);
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

  Compilation comp;

  String defaultElementNamespace = "";
  NamespaceBinding namespaceBindings = NamespaceBinding.predefinedXML;
  NamespaceBinding prologNamespaces = NamespaceBinding.predefinedXML;

  void parseSeparator ()
    throws java.io.IOException, SyntaxException
  {
    int startLine = port.getLineNumber() + 1;
    int startColumn = port.getColumnNumber() + 1;
    int next = skipSpace(nesting != 0);
    if (next == ';')
      return;
    if (warnOldVersion && next != '\n')
      {
	curLine = startLine;
	curColumn = startColumn;
	error('w', "missing ';' after declaration");
      }
    if (next >= 0)
      unread(next);
  }

  /** Parse an expression.
   * Return null on EOF. */
  public Expression parse(Compilation comp)
      throws java.io.IOException, SyntaxException
  {
    this.comp = comp;
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
	getRawToken();
	Declaration decl = parseVariableDeclaration();
	if (decl == null)
	  return syntaxError("missing Variable");
	comp.currentScope().addDeclaration(decl);
	getRawToken();
	Expression type = parseOptionalTypeDeclaration();
	comp.push(decl);
	decl.setCanRead(true);
	//decl.setFlag(Declaration.NONSTATIC_SPECIFIED);
	decl.setFlag(Declaration.IS_CONSTANT);
	Expression init = null;
	boolean sawEq = false;
	if (curToken == OP_EQU || curToken == COLON_EQUAL_TOKEN)
	  {
	    if (curToken==OP_EQU)
	      error("declare variable contains '=' instead of ':='");
	    getRawToken();
	    sawEq = true;
	  }
	if (curToken == '{')
	  {
	    if (warnOldVersion)
	      error('w', "obsolate '{' in variable declaration");
	    init = parseEnclosedExpr();
	    parseSeparator();
	  }
	else if (match("external"))
	  {
	    error("external variables not implemented yet");
	  }
	else
	  {
	    // This leave curToken pointing at the token *following* the
	    // expression, but parse is not supposed to read ahead like that.
	    // Luckily, this is only a problem in the error recovery case,
	    // when it is usually harmless.  We could fix it by supressing
	    // the getRawToken in parseMaybePrimaryExpr, which is non-trivial.
	    init = parseMaybePrimaryExpr();
	    Expression err = null;
	    if (! sawEq || init == null)
	      err = syntaxError("expected ':= init' or 'external'");
	    if (init == null)
	      init = err;
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
    if (curToken == DECLARE_NAMESPACE_TOKEN
	|| curToken == MODULE_NAMESPACE_TOKEN)
      {
	int command = curToken;
	int next = skipSpace(nesting != 0);
	if (next >= 0)
	  {
	    unread();
	    if (isNameStart((char) next))
	      {
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  return syntaxError("missing namespace prefix");
		String prefix = new String(tokenBuffer, 0, tokenBufferLength);
		getRawToken();
		if (curToken != OP_EQU)
		  return syntaxError("missing '=' in namespace declaration");
		getRawToken();
		if (curToken != STRING_TOKEN)
		  return syntaxError("missing uri in namespace declaration");
		String uri = new String(tokenBuffer, 0, tokenBufferLength);
		Declaration decl = pushNamespace(prefix, uri);
		comp.mainLambda.addDeclaration(decl);
		parseSeparator();
		if (command == MODULE_NAMESPACE_TOKEN)
		  comp.getModule().setName(Compilation.mangleURI(uri));
		SetExp sexp = new SetExp(decl, decl.getValue());
		sexp.setDefining (true);
		return sexp;
	      }
	  }
      }

    if (curToken == IMPORT_SCHEMA_TOKEN)
      return syntaxError("'import schema' not implemented");

    if (curToken == IMPORT_MODULE_TOKEN)
      {
	getRawToken();
	String prefix = null;
	if (match("namespace"))
	  {
	    getRawToken();
	    if (curToken != NCNAME_TOKEN)
	      return syntaxError("missing namespace prefix");
	    prefix = new String(tokenBuffer, 0, tokenBufferLength);
	    getRawToken();
	    if (curToken != OP_EQU)
	      return syntaxError("missing '=' in namespace declaration");
	    getRawToken();
	  }
	if (curToken != STRING_TOKEN)
	  return syntaxError("missing uri in namespace declaration");
	String uri = new String(tokenBuffer, 0, tokenBufferLength).intern();
	if (prefix != null)
	  comp.mainLambda.addDeclaration(pushNamespace(prefix, uri));
	getRawToken();
	if (match("at"))
	  {
	    getRawToken();
	    if (curToken != STRING_TOKEN)
	      return syntaxError("missing module location");
	    String at = new String(tokenBuffer, 0, tokenBufferLength);
	    parseSeparator();
	  }
	else if (curToken != ';')
	  parseSeparator();
 	ModuleExp module = comp.getModule();
	Vector forms = new Vector();
	ClassType type = ClassType.make(Compilation.mangleURI(uri));
	kawa.standard.require.importDefinitions(type, uri, forms, module, comp);
	Expression[] inits = new Expression[forms.size()];
	forms.toArray(inits);
	return BeginExp.canonicalize(inits);
      }
    if (curToken == DEFAULT_COLLATION_TOKEN)
      {
	getRawToken();
	if (curToken != STRING_TOKEN)
	  return declError("missing collation name");
	if (defaultCollator != null && ! interactive) // err:XQ0038
	    return declError("duplicate default collation declaration");
	String collation = new String(tokenBuffer, 0, tokenBufferLength);
	try
	  {
	    defaultCollator = NamedCollator.make(collation);
	  }
	catch (Exception name)
	  { // err:XQ0038
	    defaultCollator = NamedCollator.codepointCollation;
	    return declError("unknown collation '"+collation+"'");
	  }
	parseSeparator();
	return QuoteExp.voidExp;
      }
    if (curToken == DEFAULT_ELEMENT_TOKEN
	|| curToken == DEFAULT_FUNCTION_TOKEN)
      {
	boolean forFunctions = curToken == DEFAULT_FUNCTION_TOKEN;
	getRawToken();
	if (match("namespace"))
	  getRawToken();
	else
	  {
	    String msg = "expected 'namespace' keyword";
	    if (curToken != STRING_TOKEN && curToken != OP_EQU)
	      return declError(msg);
	    else if (warnOldVersion)
	      error('w', msg);
	  }
	if (curToken == OP_EQU || curToken == COLON_EQUAL_TOKEN)
	  {
	    if (warnOldVersion)
	      error('w', "extra '=' in default namespace declaration");
	    getRawToken();
	  }
	if (curToken != STRING_TOKEN)
	  return declError("missing namespace uri");
	String uri = new String(tokenBuffer, 0, tokenBufferLength);
	String prefix;
	if (forFunctions)
	  {
	    prefix = XQuery.DEFAULT_FUNCTION_PREFIX;
	    functionNamespacePath = new Namespace[1];
	    functionNamespacePath[0] = Namespace.getInstance(uri);
	  }
	else
	  {
	    prefix = XQuery.DEFAULT_ELEMENT_PREFIX;
	    defaultElementNamespace = uri;
	  }
	Declaration decl = pushNamespace(prefix, uri);
	comp.mainLambda.addDeclaration(decl);
	SetExp sexp = new SetExp(decl, decl.getValue());
	sexp.setDefining (true);
	parseSeparator();
	return sexp;
      }
    if (curToken == DECLARE_XMLSPACE_TOKEN)
      {
	getRawToken();
	if (curToken == OP_EQU)
	  {
	    if (warnOldVersion)
	      error('w', "obsolate '=' in xmlspace declaration");
	    getRawToken();
	  }
	if (match("preserve"))
	  preserveBoundarySpace = true;
	else if (match("skip"))
	  preserveBoundarySpace = false;
	else
	  return syntaxError("xmlspace declaration must be preserve or strip");
	parseSeparator();
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

	ClassType type = ClassType.make(className);
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

  static final Expression funcForwardFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "forwardFilter");
  static final Expression funcReverseFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "reverseFilter");
  static final Expression funcExprFilter
    = makeFunctionExp("gnu.xquery.util.ValuesFilter", "exprFilter");

  static final NodeType documentNodeTest
  = new NodeType("document-node", NodeType.DOCUMENT_OK);
  static final NodeType textNodeTest
    = new NodeType("text", NodeType.TEXT_OK);
  static final NodeType commentNodeTest
    = new NodeType("comment", NodeType.COMMENT_OK);
  static final NodeType piNodeTest
    = new NodeType("processing-instruction", NodeType.PI_OK);
  static final NodeType anyNodeTest
    = new NodeType("node");

  /** Helper method for debugging. */
  String tokenString()
  {
    switch (curToken)
      {
      case NCNAME_TOKEN:
      case QNAME_TOKEN:
	return new String(tokenBuffer, 0, tokenBufferLength);
      case EOF_TOKEN:
	return "<EOF>";
      default:
	return Integer.toString(curToken);
      }
  }

  public void error(char severity, String message)
  {
    error(severity, port.getName(), curLine, curColumn, message);
  }

  public Expression declError (String message)
    throws java.io.IOException, SyntaxException
  {
    if (interactive)
      return syntaxError(message);
    error(message);
    for (;;)
      {
	if (curToken==';' || curToken == EOF_TOKEN)
	  break;
	getRawToken();
      }
    return new ErrorExp (message);
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

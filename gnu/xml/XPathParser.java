// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.Convert;

public class XPathParser implements XPathConstants
{
	String str;
	int length;
	int index;
	XPath xpath;

	char curToken;
	String curName;
	int tokenStart;

	int counters;

	/** Used as a stack to remember start pc of Step forms. */
	StringBuffer stepStarts = new StringBuffer(20);

	/** Return the current token.
	 * Resolve "identifier" to 'and', 'or', 'div', 'mod'.
	 */
	char peekOperator()
	{
		if (curToken == 'A') {
			int len = index - tokenStart;
			if (len == 2 || len == 3) {
				char c1 = str.charAt(tokenStart);
				if (len == 2) {
					if (c1 == 'o' && str.charAt(tokenStart+1) == 'r')
						curToken = OP_OR;
				}
				else {
					char c2 = str.charAt(tokenStart+1);
					char c3 = str.charAt(tokenStart+2);
					if (c1 == 'a') {
						if (c2 == 'n' && c3 == 'd')
							curToken = OP_AND;
					}
					else if (c1 == 'm') {
						if (c2 == 'o' && c3 == 'd')
							curToken = OP_MOD;
					}
					else if (c1 == 'd') {
						if (c2 == 'i' && c3 == 'v')
							curToken = OP_DIV;
					}
				}
			}
		}
		return curToken;
	}

	public final static String[] axisNames = new String[COUNT_OP_AXIS];
	static {
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

	public final static String[] nodeTypeNames = {
		"node", "comment", "text", "processing-instruction" };

	public java.util.Hashtable nameSpaceTable = null;

	char peekOperand()
	{
		if (curToken == 'A') {
			int saveIndex = index;
			String name = str.substring(tokenStart, saveIndex).intern();
			char next = skipSpace();
			if (next == ':') {
				index++;
				next = index == length ? '\uffff' : str.charAt(index);
				if (next == ':') {
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
					index++;
					curName = name;
				}
				else if (index != saveIndex + 1) {
					error("invalid space after namespace operator ':'");
				}
				else if (Character.isLetter(next)) {
					int saveStart = tokenStart;
					getRawToken();
					if (nameSpaceTable == null)
						curName = str.substring(saveStart, index).intern();
					else {
						String nsalias = str.substring(tokenStart, index);
						String ns = (String) nameSpaceTable.get(nsalias);
						if (ns == null) {
							error("unknown namespace");
							curName = str.substring(saveStart, index).intern();
						}
						else {
							curName = (name + "@" + ns).intern();
						}
					}
					curToken = 'Q';
				}
				else if (next == '*') {
					// FIXME
					curToken = 'R';
				}
				else
					error("invalid character after ':'");
			}
			else {
				curToken = 'Q';
				curName = name;
			}
			if (curToken == 'Q' && next == '(') {
				name = name.intern();
				int i;
				for (i = 4;  --i >= 0; ) {
					if (nodeTypeNames[i] == name)
						break;
				}
				index++;
				if (i >= 0)
					curToken = (char) (OP_NODE + i);
				else
					curToken = 'F';  // FuncName.
			}
		}
		return curToken;
	}

	/** Skip whitspace.
	 * Sets 'index' to the that of the next non-whitespace character,
	 * and returns that.  If there are no more non-space characters,
	 * returns ' '.  */
	final char skipSpace()
	{
		int i = index;
		char ch;
		for (;; i++) {
			if (i == length) {
				ch = ' ';
				break;
			}
			ch = str.charAt(i);
			if (! Character.isWhitespace(ch)) {
				break;
			}
		}
		index = i;
		return ch;
	}

	char getRawToken()
	{
		int len = length;
		char ch = skipSpace();
		int i = index;
		if (ch == ' ') {
			ch = '\uffff';
		}
		else {
			tokenStart = index;
			i++;
			char next = i == len ? '\uffff' : str.charAt(i);
			switch (ch) {
			case '(':  case ')':  case '[':  case ']':
			case '$':  case '|':  case '@':  case ',':
				break;
			case '*':  ch = OP_MUL;  break;
			case '+':  ch = OP_ADD;  break;
			case '-':  ch = OP_SUB;  break;
			case '!':
				if (next == '=') {
					i++;
					ch = OP_NEQ;
				}
				break;
			case '/':
				if (next == '/') {
					i++;
					ch = 'D';
				}
				break;
			case '=':
				ch = OP_EQU;
				break;
			case '>':
				if (next == '=') {
					i++;
					ch = OP_GEQ;
				}
				else
					ch = OP_GRT;
				break;
			case '<':
				if (next == '=') {
					i++;
					ch = OP_LEQ;
				}
				else
					ch = OP_LSS;
				break;
			case '.':
				if (next == '.') {
					i++;
					ch = '2';
				}
				else if (Character.isDigit(next)) {
					while (++i < len) {
						next = str.charAt(i);
						if (! Character.isDigit(next))
							break;
					}
					ch = '0';
				}
				break;
			case '\'':  case '\"':
				for (;;) {
					i++;
					if (next == ch) {
						ch = '\"';
						break;
					}
					if (i == len) {
						ch = '\002';
						break;
					}
					next = str.charAt(i);
				}
				break;
			default:
				if (Character.isDigit(ch)) {
					boolean seenDot = false;
					for (;; ) {
						if (next == '.') {
							if (seenDot)  break;
							seenDot = true;
						}
						else if (! Character.isDigit(next))
							break;
						if (++i == len)
							break;
						next = str.charAt(i);
					}
					ch = '0';
				}
				else if (Character.isLetter(ch) || ch == '_') {
					for (;;) {
						if (! Character.isUnicodeIdentifierPart(next)
							&& next != '-' && next != '.')
							break;
						if (++i >= len)
							break;
						next = str.charAt(i);
					}
					ch = 'A';
				}
				else
					ch = '\001';  // invalid character
			}
			index = i;
		}
		curToken = ch;
		return ch;
	}

	public static void main(String[] args)
	{
		XPathContext context = new XPathContext();
		for (int i = 0;  i < args.length;  i++) {
			String str = args[i];
			System.out.println("String: ["+str+"]:");
			XPath xpath = new XPath();
			XPathParser parser = new XPathParser(str, xpath);

			parser.getRawToken();
			parser.parseExpr();

			System.out.print("Parsed len: "+xpath.codeLen+" {");
			for (int j = 0;  j < xpath.codeLen;  j++)
				System.out.print(" " + xpath.code[j]);
			System.out.println("}");

			double val = xpath.evalToNumber(context);
			System.out.println("value:" + val);

			/*
			for (;;) {
				char code = parser.getRawToken();
				System.out.print("- token ");
				if (code >= ' ' && code < 127)
					System.out.print("\'"+code+'\'');
				else
					System.out.print((int)code);
				System.out.println(" -> ["+
								   parser.str.substring(parser.tokenStart,parser.index)+"]");
				if (code <= '\002')
					break;
			*/
		}
	}

	public XPathParser()
	{
	}

	public XPathParser(String strpath)
	{
		init(strpath);
	}

	public void init(String strpath)
	{
		this.str = strpath;
		index = 0;
		length = strpath.length();
		counters = 0;
	}

	public XPathParser(String strpath, XPath xpath)
	{
		this(strpath);
		this.xpath = xpath;
	}

	/*
	public void parse(String path)
	{
	}
	*/

	private static final int priority(int opcode) { return opcode >> 2; }

	void parseExpr()
	{
		parseBinaryExpr(priority(OP_OR));
	}

	void parseBinaryExpr(int prio)
	{
		short start0 = xpath.codeLen;
		parseUnaryExpr();
		for (;;) {
			short start1 = xpath.codeLen;
			char token = peekOperator();
			int tokPriority = priority(token);
			if (tokPriority < prio || tokPriority > (OP_MOD >> 2))
				return;
			getRawToken();
			parseBinaryExpr(tokPriority+1);
			short start2 = xpath.codeLen;
			emit((short) (start1 - start0));
			emit((short) (start2 - start1));
			emit((short) token);
		}
	}

	void parseUnaryExpr()
	{
		if (curToken == OP_SUB) {
			getRawToken();
			parseUnionExpr();
		}
		else
			parseUnionExpr();
	}

	void parseUnionExpr()
	{
		short start0 = xpath.codeLen;
		parsePathExpr();
		if (curToken == '|') {
			StringBuffer positions = new StringBuffer(20);
			positions.append((char) start0);
			while (curToken == '|') {
				positions.append((char) xpath.codeLen);
				getRawToken();
				parsePathExpr();
			}
			int count = positions.length();
			int opcodepc = xpath.codeLen + count + 1;
			for (int i = 0;  i < count;  i++)
				emit((short) (positions.charAt(i) - opcodepc));
			emit((short) count);
			emit(OP_UNION);
		}
	}

	/** Parse a "Step" in the cpsth grammar.
	 * Returns true if a Step was seen;  false otherwise.
	 * The generated code is meant to be executed in a purely forward
	 * direction; it is *not* Reverse Polish Notation, unless wordcode
	 * as a whole.
	 */
	boolean parseStep()
	{
		short stepStart = xpath.codeLen;
		char token = peekOperand();
		boolean sawAxis = true;
		if (token == '@') {
			token = OP_AXIS_FIRST + AXIS_ATTRIBUTE;
		}
		if (token >= OP_AXIS_FIRST && token < OP_AXIS_FIRST + COUNT_OP_AXIS) {
			emit((short) token);
			getRawToken();
			token = peekOperand();
		}
		else if (token == '.') {
			emit((short) (OP_AXIS_FIRST + AXIS_SELF));
		}
		else if (token == '2') { // '..'
			emit((short) (OP_AXIS_FIRST + AXIS_PARENT));
		}
		else {
			// This may be removed if we return false.
			sawAxis = false;
			emit((short) (OP_AXIS_FIRST + AXIS_CHILD));
		}

		// parse NodeTest:
		if (token == 'Q') {
			emit(OP_SIMPLE_NAMETEST);
			emit(curName);
			getRawToken();
		}
		else if (token >= OP_NODE && token <= OP_PROCESSING_INSTRUCTION) {
			emit((short) token);
			getRawToken();
			if (token == OP_PROCESSING_INSTRUCTION) {
				if (curToken == '"') {
					emit(str.substring(tokenStart, index));
					getRawToken();
				}
				else
					emit((short) (-1));
			}
			if (curToken == ')')
				getRawToken();
			else
				error("missing ')'");
		}
		else if (token == OP_MUL) {
			emit(OP_ANY_NAMETEST);
			getRawToken();
		}
		else if (token == 'R') {
			// FIXME
			emit(OP_NAMESPACE_NAMETEST);
		}
		else if (sawAxis)
			error("missing NameTest after axis specifier");
		else {
			xpath.codeLen--; // Remove the implied child::
			return false;
		}

		while (curToken == '[') {
			getRawToken();
			emit(OP_PREDICATE);
			emit((short) ++counters);
			emit((short) 0);  // filled in later.
			int start = xpath.codeLen;
			parseExpr();
			if (curToken == ']')
				getRawToken();
			else
				error("missing ']'");
			xpath.code[start - 1] = (short) (xpath.codeLen - start);
		}
		stepStarts.append((char) stepStart);
		return true;
	}

	boolean parseOptionalRelativeLocationPath()
	{
		if (! parseStep())
			return false;
		parseRelativeLocationPathTail();
		return true;
	}

	void parseRelativeLocationPathTail()
	{
		for (;;) {
			if (curToken == 'D') {
				stepStarts.append((char) xpath.codeLen);
				emit((short) (OP_AXIS_FIRST + AXIS_DESCENDANT_OR_SELF));
				emit(OP_NODE);
			}
			else if (curToken != '/')
				break;
			getRawToken();
			if (! parseStep())
				error("missing step");
		}
	}

	void parseRelativeLocationPath()
	{
		if (!parseOptionalRelativeLocationPath())
			error("missing name step");
	}

	void parseLocationPath()
	{
		int start = xpath.codeLen;
		int prevSteps = stepStarts.length();
		if (curToken == '/') { // FIXME - wrong!  OP_ROOT is bogus!
			stepStarts.append((char) start);
			emit((short) (OP_AXIS_FIRST + AXIS_SELF));
			emit(OP_ROOT);
			getRawToken();
			parseOptionalRelativeLocationPath();
		}
		else if (curToken == 'D') { // '//'
			stepStarts.append((char) start);
			emit((short) (OP_AXIS_FIRST + AXIS_DESCENDANT_OR_SELF));
			emit(OP_NODE);
			getRawToken();
			parseRelativeLocationPath();
		}
		else
			parseRelativeLocationPath();
		emitPathLengths(prevSteps);
		emit(OP_LOCATIONPATH);
	}

	private void emitPathLengths(int prevSteps)
	{
		int numSteps = stepStarts.length()-prevSteps;
		int opcodepc = xpath.codeLen + numSteps + 1;
		for (int i = 0;  i < numSteps;  i++) {
			emit((short) (stepStarts.charAt(prevSteps+i) - opcodepc));
		}
		emit((short) numSteps);
		stepStarts.setLength(prevSteps);
	}

	void parsePathExpr()
	{
		if (parseFilterExpr()) {
			if (curToken == '/' || curToken == 'D') {
				int prevSteps = stepStarts.length();
				int start = xpath.codeLen;
				parseRelativeLocationPathTail();
				emitPathLengths(prevSteps);
				emit(OP_EXPR_LOCATIONPATH);
			}
		}
		else {
			parseLocationPath();
		}
	}

	boolean parseFilterExpr()
	{
		if (!parsePrimaryExpr())
			return false;
		while (curToken == '[') {
			getRawToken();
			parseExpr();
			if (curToken == ']')
				getRawToken();
			else
				error("missing ']'");
		}
		return true;
	}

	boolean parsePrimaryExpr()
	{
		char token = peekOperand();
		if (token == '(') {
			getRawToken();
			parseExpr();
			if (curToken == ')')
				getRawToken();
			else
				error("missing ')'");
		}
		else if (token == '"') {
			emit(str.substring(tokenStart + 1, index - 1));
			emit(OP_STRING_LITERAL);
			getRawToken();
		}
		else if (token == '0') {
			double val = Convert.parseDouble(str.substring(tokenStart, index));
			short sval = (short) val;
			if ((double) sval == val) {
				emit(sval);
				emit(OP_SHORT);
			}
			else {
				long bits = Double.doubleToLongBits(val);
				for (int i = 4;  --i >= 0; )
					emit((short) (bits >> (16 * i)));
				emit(OP_DOUBLE);
			}
			getRawToken();
		}
		else if (token == '$') {
			int start = index;
			getRawToken();
			token = peekOperand();
			if (start != tokenStart || token != 'Q')
				error("missing name after variable-name operator '$' index:"+index+" tokSt:"+tokenStart);
			emit(curName);
			emit(OP_VARIABLE_REFERENCE);
			getRawToken();
		}
		else if (token == 'F') {
			XPath xpath = this.xpath;  // Optimization.
			String fname = curName;
			getRawToken();
			short arg0Start = xpath.codeLen;
			short argCount = 0;
			if (curToken == ')')
				getRawToken();
			else
				for (;;) {
					short argStart = xpath.codeLen;
					emit((short) 0);
					parseExpr();
					xpath.code[argStart]
						= (short) (xpath.codeLen - argStart - 1);
					argCount++;
					char tok = curToken;
					if (tok != ',' && tok != ')')
						error("missing ')' after function call");
					getRawToken();
					if (tok == ')')
						break;
				}
			emit((short) (xpath.codeLen - arg0Start));
			emit(argCount);
			emit(fname);
			emit(OP_FUNCTION_CALL);
		}
		else
			return false;
		return true;
	}

	void emit(Object value)
	{
		XPath xpath = this.xpath;
		short len = xpath.valLen;
		Object[] values = xpath.values;
		if (values == null) {
			values = new Object[8];
			xpath.values = values;
		}
		for (short i = len;  --i >= 0; )
			if (values[i] == value) {
				emit(i);
				return;
			}
		if (len == values.length) {
			Object[] tmp = new Object[2 * len];
			System.arraycopy(values, 0, tmp, 0, len);
			xpath.values = tmp;
			values = tmp;
		}
		emit(len);
		values[len++] = value;
		xpath.valLen = len;
	}

	void emit(short i)
	{
		XPath xpath = this.xpath;
		short len = xpath.codeLen;
		short[] code = xpath.code;
		if (code == null) {
			code = new short[8];
			xpath.code = code;
		}
		else if (len == code.length) {
			short[] tmp = new short[2 * len];
			System.arraycopy(code, 0, tmp, 0, len);
			xpath.code = tmp;
			code = tmp;
		}
		code[len++] = i;
		xpath.codeLen = len;
	}

	public void parse()
	{
		getRawToken();
		parseExpr();
		if (curToken != '\uffff')
			error("junk at end of xpath expression: "+(int)curToken);
	}

	/**
	 * Recommended public entry point for parsing an xpath expression.
	 */
	public synchronized XPath parse(String strpath)
	{
		this.xpath = new XPath();
		init(strpath);
		parse();
		return xpath;
	}

	public static XPath parseXPath(String strpath)
	{
		XPath xpath = new XPath();
		XPathParser parser = new XPathParser(strpath, xpath);
		parser.parse();
		return xpath;
	}

	void error(String message)
	{
		throw new Error("xpath syntax error: "+message);
	}
}

// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "java"
// tab-width: 4
// indent-tabs-mode: t
// End:

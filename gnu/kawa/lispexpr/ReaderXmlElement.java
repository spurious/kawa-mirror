// Copyright (c) 2010  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import java.io.*;
import gnu.expr.*;

public class ReaderXmlElement extends ReadTableEntry
{
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    return readXMLConstructor(reader, reader.readUnicodeChar(), false);
  }

  public static Pair quote (Object obj)
  {
    Symbol q = Namespace.EmptyNamespace.getSymbol(LispLanguage.quote_sym);
    return LList.list2(q, obj);
  }

  static final String DEFAULT_ELEMENT_NAMESPACE = "[default-element-namespace]";

  /** Read either a QName literal or an enclosed QName-producing form.
   * If literal, returns a quoted symbol, and the source literal
   * in the non-empty token-buffer.
   * If non-literal, tokenBufferLength is set to 0.
   */
  public static Object readQNameExpression (LispReader reader, int ch, boolean forElement)
      throws java.io.IOException, SyntaxException
  {
    String file = reader.getName();
    int line = reader.getLineNumber() + 1;
    int column = reader.getColumnNumber();
    reader.tokenBufferLength = 0;
    if (XName.isNameStart(ch))
      {
        int colon = -1;
        for (;;)
          {
            reader.tokenBufferAppend(ch);
            ch = reader.readUnicodeChar();
            if (ch == ':' && colon < 0)
              colon = reader.tokenBufferLength;
            else if (! XName.isNamePart(ch))
              {
                reader.unread(ch);
                break;
              }
          }
        if (colon >= 0 || forElement)
          {
            int llen = reader.tokenBufferLength - colon - 1;
            String local
              = new String(reader.tokenBuffer, colon+1, llen).intern();
            String prefix = colon < 0 ? DEFAULT_ELEMENT_NAMESPACE
              : new String(reader.tokenBuffer, 0, colon).intern();
            Symbol psym = Namespace.EmptyNamespace.getSymbol(prefix);

            return new Pair(ResolveNamespace.resolveQName,
                            PairWithPosition.make(psym,
                                                  new Pair(local, LList.Empty),
                                                  reader.getName(), line, column));
          }
        else
          {
            Symbol symbol = Namespace.getDefaultSymbol(reader.tokenBufferString().intern());
            return quote(symbol);
          }
      }
    else if (ch == '{' || ch == '(')
      {
        return readEscapedExpression(reader, ch);
      }
    else
      {
        reader.error("missing element name");
        return null;
      }
  }

  static Object readEscapedExpression (LispReader reader, int ch)
    throws IOException, SyntaxException
  {
    if (ch == '(')
      {
        reader.unread(ch);
        return reader.readObject();
      }
    else
      {
        LineBufferedReader port = reader.getPort();
        char saveReadState = reader.pushNesting('{');
        int startLine = port.getLineNumber();
        int startColumn = port.getColumnNumber();
        try
          {
            Object valuesMake = new PrimProcedure(Compilation.typeValues.getDeclaredMethod("values", 1));

            Pair list = reader.makePair(valuesMake, startLine, startColumn);
            Pair last = list;
            ReadTable readTable = ReadTable.getCurrent();
            for (;;)
              {
                int line = port.getLineNumber();
                int column = port.getColumnNumber();
                ch = port.read();
                if (ch == '}')
                  break;
                if (ch < 0)
                  reader.eofError("unexpected EOF in list starting here",//FIXME
                                 startLine + 1, startColumn);
                ReadTableEntry entry = readTable.lookup(ch);
                Object value = reader.readValues(ch, entry, readTable);
                if (value == Values.empty)
                  continue;
                value = reader.handlePostfix(value, readTable, line, column);

                Pair pair = reader.makePair(value, line, column);
                reader.setCdr(last, pair);
                last = pair;
              }
            reader.tokenBufferLength = 0;
            // optimize if single
            if (last == list.getCdr())
              return last.getCar();
            return list;
          }
        finally
          {
            reader.popNesting(saveReadState);
          }
      }
  }

  /** Parse an Element or other constructs starting with '<'.
   * Assume initial '<' has been processed.
   * @param next next character (after '<').
   */
  static Object readXMLConstructor (LispReader reader, int next, boolean inElementContent)
      throws java.io.IOException, SyntaxException
  {
    Object exp;
    int startLine = reader.getLineNumber() + 1;
    int startColumn = reader.getColumnNumber() - 2;
    if (next == '!')
      {
	next = reader.read();
	if (next == '-' && (next = reader.peek()) == '-')
	  {
	    reader.skip();
	    if (! reader.readDelimited("-->"))
              reader.error('f', reader.getName(), startLine, startColumn, "unexpected end-of-file in XML comment starting here - expected \"-->\"");
            String str = reader.tokenBufferString();
            exp = LList.list2(CommentConstructor.commentConstructor, str);
	  }
        else if (next == '['
                 && (next = reader.read()) == 'C'
                 && (next = reader.read()) == 'D'
                 && (next = reader.read()) == 'A'
                 && (next = reader.read()) == 'T'
                 && (next = reader.read()) == 'A'
                 && (next = reader.read()) == '[')
	  {
	    if (! reader.readDelimited("]]>"))
              reader.error('f', reader.getName(), startLine, startColumn,
                           "unexpected end-of-file in CDATA starting here - expected \"]]>\"");
            String str = reader.tokenBufferString();
            exp = LList.list2(MakeCDATA.makeCDATA, str);
	  }
        else
          {
            reader.error('f', reader.getName(), startLine, startColumn,
                         "'<!' must be followed by '--' or '[CDATA['");
            while (next >= 0 && next != '>'
                   && next != '\n' && next != '\r')
              {
                next = reader.read();
              }
            exp = null;
          }
      }
    else if (next == '?')
      {
        next = reader.readUnicodeChar();
	if (next < 0 || ! XName.isNameStart(next))
	  reader.error("missing target after '<?'");
        for (;;)
          {
            reader.tokenBufferAppend(next);
            next = reader.readUnicodeChar();
            if (! XName.isNamePart(next))
              break;
          }
	String target = reader.tokenBufferString();
        int nspaces = 0;
        while (next >= 0 && Character.isWhitespace(next))
          {
            nspaces++;
            next = reader.read();
         }
        reader.unread(next);
        char saveReadState = reader.pushNesting('?');
        try
          {
            if (! reader.readDelimited("?>"))
              reader.error('f', reader.getName(), startLine, startColumn,
                           "unexpected end-of-file looking for \"?>\"");
          }
        finally
          {
            reader.popNesting(saveReadState);
          }
        if (nspaces == 0 && reader.tokenBufferLength > 0)
          reader.error("target must be followed by space or '?>'");
	String content = reader.tokenBufferString();
        exp = LList.list3(MakeProcInst.makeProcInst, target, content);
      }
    else
      exp = readElementConstructor(reader, next);
    return exp;
  }

  /** Parse ElementConstructor.
   * Assume initial {@code '<'} has been processed,
   * and we're read the next character.
   * Reads through end of the end tag.
   */
  public static Object readElementConstructor(LispReader reader, int ch)
      throws java.io.IOException, SyntaxException
  {
    int startLine = reader.getLineNumber() + 1;
    int startColumn = reader.getColumnNumber() - 2;
    Object tag = readQNameExpression(reader, ch, true);
    // Note that we cannot do namespace resolution at reader time,
    // because of constructs like this:  <a x="&{x:v}" xmlns:x="xx"/>
    // Instead we defer namespace lookup until rewrite-time.
    String startTag = reader.tokenBufferLength == 0 ? null
      : reader.tokenBufferString();
    Pair tagPair = PairWithPosition.make(tag, LList.Empty,
                                            reader.getName(),
                                            startLine, startColumn);
    Pair resultTail = tagPair;
    LList namespaceList = LList.Empty;
    NamespaceBinding nsBindings = null; // ???
    for (;;)
      {
        boolean sawSpace = false;
        ch = reader.readUnicodeChar();
        while (ch >= 0 && Character.isWhitespace(ch))
          {
            ch = reader.read();
            sawSpace = true;
          }
	if (ch < 0 || ch == '>' || ch == '/')
	  break;
        if (! sawSpace)
          reader.error("missing space before attribute");
        Object attrName = readQNameExpression(reader, ch, false);
	int line = reader.getLineNumber() + 1;
	int column = reader.getColumnNumber() + 1 - reader.tokenBufferLength;
	String definingNamespace = null;
        if (reader.tokenBufferLength >= 5
            && reader.tokenBuffer[0] == 'x'
            && reader.tokenBuffer[1] == 'm'
            && reader.tokenBuffer[2] == 'l'
            && reader.tokenBuffer[3] == 'n'
            && reader.tokenBuffer[4] == 's')
          {
            if (reader.tokenBufferLength == 5)
              definingNamespace = "";
            else if (reader.tokenBuffer[5] == ':')
                definingNamespace =
                  new String(reader.tokenBuffer, 6, reader.tokenBufferLength-6);
	  }
	ch = skipSpace(reader, ' ');
	if (ch != '=')
          {
            reader.error("missing '=' after attribute");
          }
	ch = skipSpace(reader, ' ');
        PairWithPosition attrList
          = PairWithPosition.make(MakeAttribute.makeAttribute, LList.Empty,
                                  reader.getName(), startLine, startColumn);
        Pair attrTail = attrList;
        PairWithPosition attrPair
          = PairWithPosition.make(attrName, LList.Empty,
                                  reader.getName(), startLine, startColumn);
        reader.setCdr(attrTail, attrPair);
        attrTail = attrPair;
        attrTail = readContent(reader, (char) ch, attrTail);
	if (definingNamespace != null)
	  {
            
            namespaceList = new PairWithPosition(attrPair,
                                                 Pair.make(definingNamespace, attrPair.getCdr()),
                                                 namespaceList);
	  }
	else
	  {
            Pair pair = PairWithPosition.make(attrList,  reader.makeNil(),
                                              null, -1, -1); // FIXME
            resultTail.setCdrBackdoor(pair);
            resultTail = pair;
	  }

      }
    boolean empty = false;
    if (ch == '/')
      {
	ch = reader.read();
	if (ch == '>')
	  empty = true;
	else
	  reader.unread(ch);
      }
    if (! empty)
      {
	if (ch != '>')
          {
            reader.error("missing '>' after start element");
            return Boolean.FALSE;
          }
	resultTail = readContent(reader, '<', resultTail);
	ch = reader.readUnicodeChar();
        if (XName.isNameStart(ch))
          {
            reader.tokenBufferLength = 0;
            for (;;)
              {
                 reader.tokenBufferAppend(ch);
                 ch = reader.readUnicodeChar();
                 if (! XName.isNamePart(ch) && ch != ':')
                   break;
              }
	    String endTag = reader.tokenBufferString();
            if (startTag == null || ! endTag.equals(startTag))
              reader.error('e', reader.getName(),
                           reader.getLineNumber() + 1,
                           reader.getColumnNumber() - reader.tokenBufferLength,
                           startTag == null
                           ? "computed start tag closed by '</"+endTag+">'"
                           : "'<"+startTag+">' closed by '</"+endTag+">'");
            reader.tokenBufferLength = 0;
          }
        ch = skipSpace(reader, ch);
	if (ch != '>')
          reader.error("missing '>' after end element");
      }
    namespaceList = LList.reverseInPlace(namespaceList);
    return PairWithPosition.make(MakeXmlElement.makeXml,
                                 Pair.make(namespaceList, tagPair),
                                 reader.getName(),
                                 startLine, startColumn);
  }

  /** Parse ElementContent (delimiter == '<')  or AttributeContent (otherwise).
   * @param delimiter is '<' if parsing ElementContent, is either '\'' or
   *   '\"' if parsing AttributeContent depending on the starting quote
   */
  public static Pair readContent(LispReader reader, char delimiter, Pair resultTail)
      throws java.io.IOException, SyntaxException
  {
    reader.tokenBufferLength = 0;
    boolean prevWasEnclosed = false;
    for (;;)
      {
        Object item = null;
        String text = null;
        int line = reader.getLineNumber() + 1;
        int column = reader.getColumnNumber();
	int next = reader.read();
        if (next < 0)
          {
            reader.error("unexpected end-of-file");
            item = Special.eof;
          }
        else if (next == delimiter)
          {
            if (delimiter == '<')
              {
                if (reader.tokenBufferLength > 0)
                  {
                    text = reader.tokenBufferString();
                    reader.tokenBufferLength = 0;
                  }
                next = reader.read();
                if (next == '/')
                  item = Special.eof;
                else
                  item = readXMLConstructor(reader, next, true);
              }
            else if (reader.checkNext(delimiter)) // ???
              {
                reader.tokenBufferAppend(delimiter);
              }
            else
              item = Special.eof;
            prevWasEnclosed = false;
          }
	else if (next == '&')
	  {
            next = reader.read();
            if (next == '#')
              readCharRef(reader);
            else if (next == '(' || next == '{')
              {
                if (reader.tokenBufferLength > 0 || prevWasEnclosed)
                  text = reader.tokenBufferString();
                reader.tokenBufferLength = 0;
                item = readEscapedExpression(reader, next);
              }
            else
              {
                item = readEntity(reader, next);
                if (prevWasEnclosed && reader.tokenBufferLength == 0)
                  text = "";
              }
            prevWasEnclosed = true;
	  }
	else
	  {
            if (delimiter != '<'
                && (next == '\t' || next == '\n' || next == '\r'))
              next = ' ';
            if (next == '<')
              reader.error('e', "'<' must be quoted in a direct attribute value");
	    reader.tokenBufferAppend((char) next);
	  }
        if (item != null && reader.tokenBufferLength > 0)
          {
            text = reader.tokenBufferString();
            reader.tokenBufferLength = 0;
          }
        if (text != null)
          {
            // Wrap String as Text to avoid extra spaces.
            // If string-insert semantcs change, may be unneeded.
            Pair tnode = Pair.list2(MakeText.makeText, text);
            Pair pair = PairWithPosition.make(tnode,  reader.makeNil(),
                                              null, -1, -1); // FIXME
            resultTail.setCdrBackdoor(pair);
            resultTail = pair;
          }
        if (item == Special.eof)
          break;
        if (item != null)
          {
            Pair pair = PairWithPosition.make(item,  reader.makeNil(),
                                              null, line, column);
            resultTail.setCdrBackdoor(pair);
            resultTail = pair;
          }
      }
    return resultTail;
  }

  /** Read a character reference, assuming {@code "&#"} have been read. */
  static void readCharRef (LispReader reader)
    throws IOException, SyntaxException
  {
    int base;
    int next = reader.read();
    if (next == 'x')
      {
        base = 16;
        next = reader.read();
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
        next = reader.read();
      }
    if (next != ';')
      {
        reader.unread(next);
        reader.error("invalid character reference");
      }
    // See definition of 'Char' in XML 1.1 2nd ed Specification.
    else if ((value > 0 && value <= 0xD7FF)
             || (value >= 0xE000 && value <= 0xFFFD)
             || (value >= 0x10000 && value <= 0x10FFFF))
      {
        reader.tokenBufferAppend(value);
      }
    else
      reader.error("invalid character value "+value);
  }

  /** Read entity following {@code '&'}.
   * If result is null, it was appended to the token-buffer.
   */
  static Object readEntity (LispReader reader, int next)
    throws IOException, SyntaxException
  {
    Object result = "?";
    // FIXME
      {
	int saveLength = reader.tokenBufferLength;
	while (next >= 0)
	  {
	    char ch = (char) next;
	    if (! XName.isNamePart(ch))
	      break;
	    reader.tokenBufferAppend(ch);
	    next = reader.read();
	  }
	if (next != ';')
	  {
            reader.unread(next);
            reader. error("invalid entity reference");
	  }
        else
          {
            String ref = new String(reader.tokenBuffer, saveLength,
                                    reader.tokenBufferLength - saveLength);
            reader.tokenBufferLength = saveLength;
            namedEntity(reader, ref);
            result = null;
          }
      }
    return result;
  }

  public static void namedEntity(LispReader reader, String name)
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
      reader.error("unknown enity reference: '"+name+"'");
    reader.tokenBufferAppend(ch);
    return;
  }

  static int skipSpace (LispReader reader, int ch)
      throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
        if (ch < 0 || ! Character.isWhitespace(ch))
          return ch;
        ch = reader.readUnicodeChar();
      }
  }
}

// Copyright (c) 2001, 2003, 2005, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.*;
import gnu.text.Char;
import gnu.math.RealNum;
import gnu.text.PrettyWriter;
import gnu.mapping.OutPort;
import gnu.mapping.ThreadLocation;
import gnu.mapping.Symbol;
import java.math.BigDecimal;
import gnu.expr.Keyword;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter extends OutPort
  implements PositionConsumer, XConsumer
{
  /** Controls whether to add extra indentation.
   * -1: don't add indentation; 0: pretty-print (avoid needless newlines);
   * 1: indent (force). */
  public int printIndent = -1;
  /** When indentating, should attributes be lined up? */
  public boolean indentAttributes;

  boolean printXMLdecl = false;
  public void setPrintXMLdecl (boolean value) { printXMLdecl = value; }
  boolean inDocument;
  boolean inAttribute = false;
  boolean inStartTag = false;
  /** 0: not in comment; 1: in comment normal; 2: in comment after '-'. */
  int inComment;
  boolean needXMLdecl = false;
  boolean canonicalize = true;
  public boolean canonicalizeCDATA;
  /** Handling of empty elements.
   * 0: No element element tags, as required for canonical XML:
   * {@code <br></br>}.
   * 1: Use XML-style empty element tags: {@code <br/>}
   * 1: Use HTML-compatible empty element tags: {@code <br />}
   */
  public int useEmptyElementTag = 2;
  public boolean escapeText = true;
  public boolean escapeNonAscii = true;
  boolean isHtml = false;
  boolean undeclareNamespaces = false;
  Object style;
  /** Fluid parameter to control whether a DOCTYPE declaration is emitted.
   * If non-null, this is the the public identifier. */
  public static final ThreadLocation doctypeSystem
    = new ThreadLocation("doctype-system");
  /** The system identifier emitted in a DOCTYPE declaration. 
   * Has no effect if doctypeSystem returns null.
   * If non-null, this is the the system identifier. */
  public static final ThreadLocation doctypePublic
    = new ThreadLocation("doctype-public");
  public static final ThreadLocation indentLoc
    = new ThreadLocation("xml-indent");

  /** Chain of currently active namespace nodes. */
  NamespaceBinding namespaceBindings = NamespaceBinding.predefinedXML;

  /** Stack of namespaceBindings as of active beginGroup calls. */
  NamespaceBinding[] namespaceSaveStack = new NamespaceBinding[20];

  Object[] groupNameStack = new Object[20];

  /** Difference between number of beginGroup and endGroup calls so far. */
  int groupNesting;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  private static final int ELEMENT_START = -3;
  private static final int ELEMENT_END = -4;
  private static final int COMMENT = -5;
  private static final int KEYWORD = -6;
  int prev = ' ';

  char savedHighSurrogate; // should perhaps be combined with prev?

  public XMLPrinter (OutPort out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (Writer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (OutputStream out, boolean autoFlush)
  {
    super(new OutputStreamWriter(out), true, autoFlush);
  }

  public XMLPrinter (Writer out)
  {
    super(out);
  }

  public XMLPrinter (OutputStream out)
  {
    super(new OutputStreamWriter(out), false, false);
  }

  public XMLPrinter (OutputStream out, String fname)
  {
    super(new OutputStreamWriter(out), true, false, fname);
  }

  public static XMLPrinter make(OutPort out, Object style)
  {
    XMLPrinter xout = new XMLPrinter(out, true);
    xout.setStyle(style);
    return xout;
  }

  /** Convert argument to string in XML syntax. */

  public static String toString (Object value)
  {
    StringWriter stringWriter = new StringWriter();
    new XMLPrinter(stringWriter).writeObject(value);
    return stringWriter.toString();
  }

  public void setStyle (Object style)
  {
    this.style = style;
    useEmptyElementTag = canonicalize ? 0 : 1;
    if ("html".equals(style))
      {
	isHtml = true;
	useEmptyElementTag = 2;
      }
    if ("xhtml".equals(style))
      useEmptyElementTag = 2;
    if ("plain".equals(style))
      escapeText = false;
  }

  public void write (int v)
  {
    closeTag();
    if (printIndent >= 0)
      {
        if ((v == '\r' || v == '\n'))
          {
            if (v != '\n' || prev != '\r')
              writeBreak(PrettyWriter.NEWLINE_MANDATORY);
            if (inComment > 0)
              inComment = 1;
            return;
          }
      }
    if (! escapeText)
      {
	bout.write(v);
	prev = v;
      }
    else if (inComment > 0)
      {
        if (v == '-')
          {
            if (inComment == 1)
              inComment = 2;
            else
              bout.write(' ');
          }
        else
          inComment = 1;
        super.write(v);
      }
    else
      {
	prev = ';';
	if (v == '<' && ! (isHtml && inAttribute))
	  bout.write("&lt;");
	else if (v == '>')
	  bout.write("&gt;");
	else if (v == '&')
	  bout.write("&amp;");
	else if (v == '\"' && inAttribute)
	  bout.write("&quot;");
	else if ((escapeNonAscii && v >= 127)
                 // We must escape control characters in attributes,
                 // since otherwise they get normalized to ' '.
                 || (v < ' ' && (inAttribute || (v != '\t' && v != '\n'))))
          {
            int i = v;
            if (v >= 0xD800)
              {
                if (v < 0xDC00)
                  {
                    savedHighSurrogate = (char) v;
                    return;
                  }
                else if (v < 0xE000)
                  { // low surrogate
                    //if (highSurrogate < 0xDC00 || highSurrogate > 0xE000)
                    // error();
                    i = (savedHighSurrogate - 0xDC00) * 0x400
                      + (i - 0xDC00) + 0x10000;
                    savedHighSurrogate = 0;
                  }
              }
            bout.write("&#x"+Integer.toHexString(i).toUpperCase()+";");
          }
	else
	  {
	    bout.write(v);
	    prev = v;
	  }
      }
  }

  private void startWord()
  {
    closeTag();
    writeWordStart();
  }

  public void writeBoolean(boolean v)
  {
    startWord();
    super.print(v);
    writeWordEnd();
  }

  protected void startNumber()
  {
    startWord();
  }

  protected void endNumber()
  {
    writeWordEnd();
  }

  public void closeTag()
  {
    if (inStartTag && ! inAttribute)
      {
	if (printIndent >= 0 && indentAttributes)
          endLogicalBlock("");
	bout.write('>');
	inStartTag = false;
	prev = ELEMENT_START;
      }
    else if (needXMLdecl)
      {
	// should also include encoding declaration FIXME.
	bout.write("<?xml version=\"1.0\"?>\n");
	if (printIndent >= 0)
	  {
	    startLogicalBlock("", "", 2);
	  }
	needXMLdecl = false;
      }
  }

  void setIndentMode ()
  {
    Object xmlIndent = indentLoc.get(null);
    String indent = xmlIndent == null ? null : xmlIndent.toString();
    if (indent == null)
      printIndent = -1;
    else if (indent.equals("pretty"))
      printIndent = 0;
    else if (indent.equals("always") || indent.equals("yes"))
      printIndent = 1;
    else // if (ident.equals("no")) or default:
      printIndent = -1;
  }

  public void beginDocument()
  {
    if (printXMLdecl)
      {
	// We should emit an XML declaration, but don't emit it yet, in case
	// we get it later as a processing instruction.
	needXMLdecl = true;
      }
    setIndentMode();
    inDocument = true;
    if (printIndent >= 0 && ! needXMLdecl)
      startLogicalBlock("", "", 2);
  }

  public void endDocument()
  {
    inDocument = false;
    if (printIndent >= 0)
      endLogicalBlock("");
    freshLine();
  }

  public void beginEntity (Object base)
  {
  }

  public void endEntity ()
  {
  }

  protected void writeQName (Object name)
  {
    if (name instanceof Symbol)
      {
        Symbol sname = (Symbol) name;
        String prefix = sname.getPrefix();
        if (prefix != null && prefix.length() > 0)
          {
            bout.write(prefix);
            bout.write(':');
          }
        bout.write(sname.getLocalPart());
      }
    else
      bout.write(name == null ? "{null name}" : (String) name);
  }

  public void beginGroup(Object type)
  {
    closeTag();
    if (groupNesting == 0)
      {
        if (! inDocument)
          setIndentMode();
        Object systemIdentifier = doctypeSystem.get(null);
        if (systemIdentifier != null)
          {
            String systemId = systemIdentifier.toString();
            if (systemId.length() > 0)
              {
                Object publicIdentifier = doctypePublic.get(null);
                bout.write("<!DOCTYPE ");
                bout.write(type.toString());
                String publicId = publicIdentifier == null ? null
                  : publicIdentifier.toString();
                if (publicId != null && publicId.length() > 0)
                  {
                    bout.write(" PUBLIC \"");
                    bout.write(publicId);
                    bout.write("\" \"");
                  }
                else
                  {
                    bout.write(" SYSTEM \"");
                  }
                bout.write(systemId);
                bout.write("\">");
                println();
              }
          }
      }
    if (printIndent >= 0)
      {
	if (prev == ELEMENT_START || prev == ELEMENT_END || prev == COMMENT)
	  writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
			  : PrettyWriter.NEWLINE_LINEAR);
	startLogicalBlock("", "", 2);
      }
    bout.write('<');
    writeQName(type);
    if (printIndent >= 0 && indentAttributes)
      startLogicalBlock("", "", 2);
    groupNameStack[groupNesting] = type;
    NamespaceBinding groupBindings = null;
    namespaceSaveStack[groupNesting++] = namespaceBindings;
    if (type instanceof XName)
      {
	groupBindings = ((XName) type).namespaceNodes;
	NamespaceBinding join
	  = NamespaceBinding.commonAncestor(groupBindings, namespaceBindings);
        int numBindings = groupBindings == null ? 0
          : groupBindings.count(join);
        NamespaceBinding[] sortedBindings = new NamespaceBinding[numBindings];
        int i = 0;
        boolean sortNamespaces = canonicalize;
      check_namespaces:
	for (NamespaceBinding ns = groupBindings;  ns != join;  ns = ns.next)
          {
            int j = i;
            boolean skip = false;
            String uri = ns.getUri();
            String prefix = ns.getPrefix();
            while (--j >= 0)
              {
                NamespaceBinding ns_j = sortedBindings[j];
                // If (compare(ns, ns_j) <= 0) break:
                String prefix_j = ns_j.getPrefix();
                if (prefix == prefix_j)
                  continue check_namespaces;
                // If we're not canonicalizing, we just want to suppress
                // duplicates, rather than putting them in order.
                // Note we put the bindings in reverse order, since that's
                // the following print loop expects.
                if (! sortNamespaces)
                  continue;
                if (prefix == null)
                  break;
                if (prefix_j != null && prefix.compareTo(prefix_j) <= 0)
                  break;
                sortedBindings[j+1] = ns_j;
              }
            if (sortNamespaces)
              j++;
            else
              j = i;
            sortedBindings[j] = ns;
            i++;
          }
        numBindings = i;
        // Note we print the bindings in reverse order, since the chain
        // is in reverse document order.
        for (i = numBindings; --i >= 0; )
	  {
            NamespaceBinding ns = sortedBindings[i];
	    String prefix = ns.prefix;
	    String uri = ns.uri;
	    if (uri == namespaceBindings.resolve(prefix))
	      // A matching namespace declaration is already in scope.
	      continue;
	    bout.write(' '); // prettyprint break
	    if (prefix == null)
	      bout.write("xmlns");
	    else
	      {
		bout.write("xmlns:");
		bout.write(prefix);
	      }
	    bout.write("=\"");
	    inAttribute = true;
	    if (uri != null)
	      write(uri);
	    inAttribute = false;
	    bout.write('\"');
	  }
	if (undeclareNamespaces)
	  {
	    // As needed emit namespace undeclarations as in
	    // the XML Namespaces 1.1 Candidate Recommendation.
	    // Most commonly this loop will run zero times.
	    for (NamespaceBinding ns = namespaceBindings;
		 ns != join;  ns = ns.next)
	      {
		String prefix = ns.prefix;
		if (ns.uri != null && groupBindings.resolve(prefix) == null)
		  {
		    bout.write(' '); // prettyprint break
		    if (prefix == null)
		      bout.write("xmlns");
		    else
		      {
			bout.write("xmlns:");
			bout.write(prefix);
		      }
		    bout.write("=\"\"");
		  }
	      }
	  }
	namespaceBindings = groupBindings;
      }
    if (groupNesting >= namespaceSaveStack.length)
      {
	NamespaceBinding[] nstmp = new NamespaceBinding[2 * groupNesting];
	System.arraycopy(namespaceSaveStack, 0, nstmp, 0, groupNesting);
	namespaceSaveStack = nstmp;
	Object[] nmtmp = new Object[2 * groupNesting];
	System.arraycopy(groupNameStack, 0, nmtmp, 0, groupNesting);
	groupNameStack = nmtmp;
      }

    inStartTag = true;
    if (isHtml)
      {
        String typeName = (type instanceof Symbol
                           ? ((Symbol) type).getLocalPart()
                           : type.toString());
	if ("script".equals(typeName) || "style".equals(typeName))
          escapeText = false;
      }
  }

  static final String HtmlEmptyTags
  = "/area/base/basefont/br/col/frame/hr/img/input/isindex/link/meta/para/";

  public static boolean isHtmlEmptyElementTag(String name)
  {
    int index = HtmlEmptyTags.indexOf(name);
    return index > 0 && HtmlEmptyTags.charAt(index-1) == '/'
      && HtmlEmptyTags.charAt(index+name.length()) == '/';
  }

  public void endGroup ()
  {
    if (useEmptyElementTag == 0)
      closeTag();
    Object type = groupNameStack[groupNesting-1];
    String typeName = ! isHtml ? null // not needed
      : type instanceof Symbol ? ((Symbol) type).getLocalPart()
      : type.toString();
    if (inStartTag)
      {
	if (printIndent >= 0 && indentAttributes)
	  {
	    endLogicalBlock("");
	  }
	bout.write(isHtml
		 ? (isHtmlEmptyElementTag(typeName) ? ">" : "></"+typeName+">")
		 : (useEmptyElementTag == 2 ? " />" : "/>"));
	inStartTag = false;
      }
    else
      {
	if (printIndent >= 0)
	  {
	    setIndentation(0, false);
	    if (prev == ELEMENT_END)
	      writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
                         : PrettyWriter.NEWLINE_LINEAR);
	  }
	bout.write("</");
        writeQName(type);
	bout.write(">");
      }
    if (printIndent >= 0)
      {
	endLogicalBlock("");
      }
    prev = ELEMENT_END;
    if (isHtml && ! escapeText
	&& ("script".equals(typeName) || "style".equals(typeName)))
      escapeText = true;

    namespaceBindings = namespaceSaveStack[--groupNesting];
    namespaceSaveStack[groupNesting] = null;
    groupNameStack[groupNesting] = null;
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(Object attrType)
  {
    if (inAttribute)
      bout.write('"');
    inAttribute = true;
    bout.write(' ');
    if (printIndent >= 0)
      writeBreakFill();
    bout.write(attrType.toString()); // FIXME: use Symbol.print
    bout.write("=\"");
    prev = ' ';
  }

  public void endAttribute()
  {
    if (inAttribute)
      {
        if (prev != KEYWORD)
          {
            bout.write('"');
            inAttribute = false;
          }
        prev = ' ';
      }
  }

  public void writeDouble (double d)
  {
    startWord();
    bout.write(formatDouble(d));
  }

  public void writeFloat (float f)
  {
    startWord();
    bout.write(formatFloat(f));
  }

  /** Helper to format xs:double according to XPath/XQuery specification. */
  public static String formatDouble (double d)
  {
    if (Double.isNaN(d))
      return "NaN";
    boolean neg = d < 0;
    if (Double.isInfinite(d))
      return neg ? "-INF" : "INF";
    double dabs = neg ? -d : d;
    String dstr = Double.toString(d);
    // Unfortunately, XQuery's rules for when to use scientific notation
    // are different from Java's.  So fixup the string, if needed.
    if ((dabs >= 1000000 || dabs < 0.000001) && dabs != 0.0)
      return RealNum.toStringScientific(dstr);
    else
      return formatDecimal(RealNum.toStringDecimal(dstr));
  }

  /** Helper to format xs:float according to XPath/XQuery specification. */
  public static String formatFloat (float f)
  {
    if (Float.isNaN(f))
      return "NaN";
    boolean neg = f < 0;
    if (Float.isInfinite(f))
      return neg ? "-INF" : "INF";
    float fabs = neg ? -f : f;
    String fstr = Float.toString(f);
    // Unfortunately, XQuery's rules for when to use scientific notation
    // are different from Java's.  So fixup the string, if needed.
    if ((fabs >= 1000000 || fabs < 0.000001) && fabs != 0.0)
      return RealNum.toStringScientific(fstr);
    else
      return formatDecimal(RealNum.toStringDecimal(fstr));
  }

  /** Format java.math.BigDecimal as needed for XPath/XQuery's xs:decimal.
   * Specifically this means removing trailing fractional zeros, and a trailing
   * decimal point. However, note that the XML Schema canonical representation
   * does require a decimal point and at least one fractional digit.
   */
  public static String formatDecimal (BigDecimal dec)
  {
    /* #ifdef JAVA5 */
    // return formatDecimal(dec.toPlainString());
    /* #else */
    return formatDecimal(dec.toString());
    /* #endif */
  }

  static String formatDecimal (String str)
  {
    int dot = str.indexOf('.');
    if (dot >= 0)
      {
        int len = str.length();
        for (int pos = len; ; )
          {
            char ch = str.charAt(--pos);
            if (ch != '0')
              {
                if (ch != '.')
                  pos++;
                return pos == len ? str : str.substring(0, pos);
              }
          }
      }
    return str;
  }

  public void print(Object v)
  {
    if (v instanceof BigDecimal)
      v = formatDecimal((BigDecimal) v);
    else if (v instanceof Double || v instanceof gnu.math.DFloNum)
      v = formatDouble(((Number) v).doubleValue());
    else if (v instanceof Float)
      v = formatFloat(((Float) v).floatValue());
    write(v == null ? "(null)" : v.toString());
  }

  public void writeObject(Object v)
  {
    if (v instanceof SeqPosition)
      {
        bout.clearWordEnd();
	SeqPosition pos = (SeqPosition) v;
	pos.sequence.consumeNext(pos.ipos, this);
        if (pos.sequence instanceof NodeTree)
          prev = '-';
	return;
      }
    if (v instanceof Consumable && ! (v instanceof UnescapedData))
      {
	((Consumable) v).consume(this);
	return;
      }
    if (v instanceof Keyword)
      {
        beginAttribute(((Keyword) v).getName());
        prev = KEYWORD;
        return;
      }
    closeTag();
    if (v instanceof UnescapedData)
      {
        bout.clearWordEnd();
	bout.write(((UnescapedData) v).getData());
        prev = '-';
      }
    else if (v instanceof Char)
      Char.print(((Char) v).intValue(), this);
    else
      {
	startWord();
	prev = ' ';
	print(v);
        writeWordEnd();
	prev = WORD;
      }
  }

  /** Write each element of a sequence, which can be an array,
   * a Sequence or a Consumable. */
  //public void writeAll(Object sequence);

  /** True if consumer is ignoring rest of group.
   * The producer can use this information to skip ahead. */
  public boolean ignoring()
  {
    return false;
  }

  public void write (String str, int start, int length)
  {
    if (length > 0)
      {
        closeTag();
        int limit = start + length;
        int count = 0;
        while (start < limit)
          {
            char c = str.charAt(start++);
            if (c >= 127 || c == '\n' || c == '\r'
                || (inComment > 0 ? (c == '-' || inComment == 2)
                    : (c == '<' || c == '>' || c == '&'
                       || (inAttribute && (c == '"' || c < ' ' )))))
              {
                if (count > 0)
                  bout.write(str, start - 1 - count, count);
                write(c);
                count = 0;
              }
            else
              count++;
          }
        if (count > 0)
          bout.write(str, limit - count, count);
      }
    prev = '-';
  }

  public void write(char[] buf, int off, int len)
  {
    if (len > 0)
      {
        closeTag();
        int limit = off + len;
        int count = 0;
        while (off < limit)
          {
            char c = buf[off++];
            if (c >= 127 || c == '\n' || c == '\r'
                || (inComment > 0 ? (c == '-' || inComment == 2)
                    : (c == '<' || c == '>' || c == '&'
                       || (inAttribute && (c == '"' || c < ' ' )))))
              {
                if (count > 0)
                  bout.write(buf, off - 1 - count, count);
                write(c);
                count = 0;
              }
            else
              count++;
          }
        if (count > 0)
          bout.write(buf, limit - count, count);
      }
    prev = '-';
  }

  public void writePosition(AbstractSequence seq, int ipos)
  {
    seq.consumeNext(ipos, this);
  }

  public void writeBaseUri (Object uri)
  {
  }

  public void beginComment ()
  {
    closeTag();
    if (printIndent >= 0)
      {
	if (prev == ELEMENT_START || prev == ELEMENT_END || prev == COMMENT)
	  writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
                     : PrettyWriter.NEWLINE_LINEAR);
      }
    bout.write("<!--");
    inComment = 1;
  }

  public void endComment ()
  {
    bout.write("-->");
    prev = COMMENT;
    inComment = 0;
  }

  public void writeComment(String chars)
  {
    beginComment();
    write(chars);
    endComment();
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    beginComment();
    write(chars, offset, length);
    endComment();
  }

  public void writeCDATA (char[] chars, int offset, int length)
  {
    if (canonicalizeCDATA)
      {
        write(chars, offset, length);
        return;
      }
    closeTag();
    bout.write("<![CDATA[");
    int limit = offset+length;
    // Look for and deal with embedded "]]>".  This can't happen with
    // data generated from XML, but maybe somebody generated invalid CDATA.
    for (int i = offset;  i < limit - 2;  i++)
      {
	if (chars[i] == ']' && chars[i+1] == ']' && chars[i+2] == '>')
	  {
	    if (i > offset)
	      bout.write(chars, offset, i - offset);
	    print("]]]><![CDATA[]>");
	    offset = i + 3;
	    length = limit - offset;
	    i = i + 2;
	  }
      }
    bout.write(chars, offset, length);
    bout.write("]]>");
    prev = '>';
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    if ("xml".equals(target))
      needXMLdecl = false;
    closeTag();
    bout.write("<?");
    print(target);
    print(' ');
    bout.write(content, offset, length);
    bout.write("?>");
    prev = '>';
  }

  public void consume (SeqPosition position)
  {
    position.sequence.consumeNext(position.ipos, this);
  }
}

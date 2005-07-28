// Copyright (c) 2001, 2003, 2005  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.*;
import gnu.text.Char;
import gnu.text.PrettyWriter;
import gnu.mapping.OutPort;
import gnu.mapping.ThreadLocation;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter extends PrintConsumer
  implements PositionConsumer, XConsumer
{
  /** Controls whether to add extra indentation.
   * -1: don't add indentation; 0: pretty-print (avoid needless newlines);
   * 1: indent (force). */
  int printIndent = -1;

  boolean printXMLdecl = false;
  boolean inAttribute = false;
  boolean inStartTag = false;
  boolean needXMLdecl = false;
  boolean canonicalize = true;
  boolean htmlCompat = true;
  public boolean escapeText = true;
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

  /** Difference between number of beginGroup and endGroup calls so far. */
  int groupNesting;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  private static final int ELEMENT_START = -3;
  private static final int ELEMENT_END = -4;
  int prev = ' ';

  public XMLPrinter (Writer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (Consumer out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  /** To disambiguate between Writer and Consumer versions. */
  public XMLPrinter (PrintConsumer out, boolean autoFlush)
  {
    super((Writer) out, autoFlush);
  }

  public XMLPrinter (OutputStream out, boolean autoFlush)
  {
    super(out, autoFlush);
  }

  public XMLPrinter (Consumer out)
  {
    super(out, false);
  }

  public XMLPrinter (Writer out)
  {
    super(out);
  }

  public XMLPrinter (PrintConsumer out)
  {
    super((Writer) out, false);
  }

  public XMLPrinter (OutputStream out)
  {
    super(out, false);
  }

  public static XMLPrinter make(Consumer out, Object style)
  {
    XMLPrinter xout = new XMLPrinter(out);
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
    htmlCompat = false;
    if ("html".equals(style))
      {
	isHtml = true;
	htmlCompat = true;
      }
    if ("xhtml".equals(style))
      htmlCompat = true;
    if ("plain".equals(style))
      escapeText = false;
  }

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  public void writeChar(int v)
  {
    closeTag();
    if (printIndent >= 0 && (v == ' ' || v == '\t'))
      {
        ((OutPort) out).writeSpaceFill();
        prev = ' ';
	return;
      }
    if (printIndent >= 0 && (v == '\r' || v == '\n'))
      {
	if (v != '\n' || prev != '\r')
	  ((OutPort) out).writeBreak(PrettyWriter.NEWLINE_MANDATORY);
	return;
      }
    // if (v >= 0x10000) emit surrogtes FIXME;
    if (! escapeText)
      {
	super.write((char) v);
	prev = v;
      }
    else
      {
	prev = ';';
	if (v == '<' && ! (isHtml && inAttribute))
	  super.write("&lt;");
	else if (v == '>')
	  super.write("&gt;");
	else if (v == '&')
	  super.write("&amp;");
	else if (v == '\"' && inAttribute)
	  super.write("&quot;");
	else if (v >= 127)
	  super.write("&#"+v+";");
	else
	  {
	    super.write((char) v);
	    prev = v;
	  }
      }
  }

  private void startWord()
  {
    closeTag();
    if (prev == WORD)
      super.write(' ');
    prev = WORD;
  }

  public void writeBoolean(boolean v)
  {
    startWord();
    super.print(v);
  }

  private void closeTag()
  {
    if (inStartTag && ! inAttribute)
      {
	if (printIndent >= 0)
	  {
	    ((OutPort) out).endLogicalBlock("");
	  }
	super.write('>');
	inStartTag = false;
	prev = ELEMENT_START;
      }
    else if (needXMLdecl)
      {
	// should also include encoding declaration FIXME.
	super.write("<?xml version=\"1.0\"?>\n");
	if (printIndent >= 0)
	  {
	    ((OutPort) out).startLogicalBlock("", "", 2);
	  }
	needXMLdecl = false;
      }
  }

  protected void startNumber()
  {
    startWord();
  }

  public void beginDocument()
  {
    if (printXMLdecl)
      {
	// We should emit an XML declaration, but don't emit it set, in case
	// we get it later as a processing instruction.
	needXMLdecl = true;
      }
    if (printIndent >= 0 && ! needXMLdecl)
      ((OutPort) out).startLogicalBlock("", "", 2);
  }

  public void endDocument()
  {
    if (printIndent >= 0)
      ((OutPort) out).endLogicalBlock("");
  }

  public void beginGroup(String typeName, Object type)
  {
    closeTag();
    if (groupNesting == 0)
      {
        if (out instanceof OutPort)
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
        else
          printIndent = -1;

        Object systemIdentifier = doctypeSystem.get(null);
        if (systemIdentifier != null)
          {
            String systemId = systemIdentifier.toString();
            if (systemId.length() > 0)
              {
                Object publicIdentifier = doctypePublic.get(null);
                super.write("<!DOCTYPE ");
                super.write(typeName);
                String publicId = publicIdentifier == null ? null
                  : publicIdentifier.toString();
                if (publicId != null && publicId.length() > 0)
                  {
                    super.write(" PUBLIC \"");
                    super.write(publicId);
                    super.write("\" \"");
                  }
                else
                  {
                    super.write(" SYSTEM \"");
                  }
                super.write(systemId);
                super.write("\">");
                if (out instanceof OutPort)
                  ((OutPort) out).println();
              }
          }
      }
    if (printIndent >= 0)
      {
	OutPort pout = (OutPort) out;
	if (prev == ELEMENT_START || prev == ELEMENT_END)
	  pout.writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
			  : PrettyWriter.NEWLINE_LINEAR);
	pout.startLogicalBlock("", "", 2);
      }
    super.write('<');
    super.write(typeName);
    NamespaceBinding groupBindings = null;
    namespaceSaveStack[groupNesting++] = namespaceBindings;
    if (type instanceof XName)
      {
	groupBindings = ((XName) type).namespaceNodes;
	NamespaceBinding join
	  = NamespaceBinding.commonAncestor(groupBindings, namespaceBindings);
	for (NamespaceBinding ns = groupBindings;  ns != join;  ns = ns.next)
	  {
	    String prefix = ns.prefix;
	    String uri = ns.uri;
	    if (uri == namespaceBindings.resolve(prefix)
		|| uri == groupBindings.resolve(prefix, ns))
	      // A matching namespace declaration is already in scope.
	      continue;
	    super.write(' '); // prettyprint break
	    if (prefix == null)
	      super.write("xmlns");
	    else
	      {
		super.write("xmlns:");
		super.write(prefix);
	      }
	    super.write("=\"");
	    inAttribute = true;
	    if (uri != null)
	      writeChars(uri);
	    inAttribute = false;
	    super.write('\"');
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
		    super.write(' '); // prettyprint break
		    if (prefix == null)
		      super.write("xmlns");
		    else
		      {
			super.write("xmlns:");
			super.write(prefix);
		      }
		    super.write("=\"\"");
		  }
	      }
	  }
	namespaceBindings = groupBindings;
      }
    if (groupNesting >= namespaceSaveStack.length)
      {
	NamespaceBinding[] tmp = new NamespaceBinding[2 * groupNesting];
	System.arraycopy(namespaceSaveStack, 0, tmp, 0, groupNesting);
	namespaceSaveStack = tmp;
      }

    inStartTag = true;
    if (isHtml
	&& ("script".equals(typeName) || "style".equals(typeName)))
      escapeText = false;
    if (printIndent >= 0)
      ((OutPort) out).startLogicalBlock("", "", 1);
  }

  static final String HtmlEmptyTags
  = "/area/base/basefont/br/col/frame/hr/img/input/isindex/link/meta/para/";

  public static boolean isHtmlEmptyElementTag(String name)
  {
    int index = HtmlEmptyTags.indexOf(name);
    return index > 0 && HtmlEmptyTags.charAt(index-1) == '/'
      && HtmlEmptyTags.charAt(index+name.length()) == '/';
  }

  public void endGroup(String typeName)
  {
    if (canonicalize && ! htmlCompat)
      closeTag();
    if (inStartTag)
      {
	if (printIndent >= 0)
	  {
	    ((OutPort) out).endLogicalBlock("");
	  }
	super.write(isHtml
		 ? (isHtmlEmptyElementTag(typeName) ? ">" : "></"+typeName+">")
		 : (htmlCompat ? " />" : "/>"));
	inStartTag = false;
      }
    else
      {
	if (printIndent >= 0)
	  {
	    OutPort pout = (OutPort) out;
	    pout.setIndentation(0, false);
	    if (prev == ELEMENT_END)
	      pout.writeBreak(printIndent > 0 ? PrettyWriter.NEWLINE_MANDATORY
			      : PrettyWriter.NEWLINE_LINEAR);
	  }
	super.write("</");
	super.write(typeName);
	super.write(">");
      }
    if (printIndent >= 0)
      {
	OutPort pout = (OutPort) out;
	pout.endLogicalBlock("");
      }
    prev = ELEMENT_END;
    if (isHtml && ! escapeText
	&& ("script".equals(typeName) || "style".equals(typeName)))
      escapeText = true;

    namespaceBindings = namespaceSaveStack[--groupNesting];
    namespaceSaveStack[groupNesting] = null;
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(String attrName, Object attrType)
  {
    if (inAttribute)
      super.write('"');
    inAttribute = true;
    super.write(' ');
    if (printIndent >= 0)
      ((OutPort) out).writeBreakFill();
    super.write(attrName);
    super.write("=\"");
    prev = ' ';
  }

  public void endAttribute()
  {
    super.write('"');
    inAttribute = false;
    prev = ' ';
  }

  public void writeObject(Object v)
  {
    if (v instanceof Consumable && ! (v instanceof UnescapedData))
      {
	((Consumable) v).consume(this);
	return;
      }
    if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	pos.sequence.consumeNext(pos.ipos, this);
	return;
      }
    closeTag();
    if (v instanceof UnescapedData)
      {
	super.write(((UnescapedData) v).getData());
      }
    else if (v instanceof Char)
      writeChar(((Char) v).intValue());
    else
      {
	startWord();
	prev = ' ';
	writeChars(v == null ? "(null)" : v.toString());
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

  public void writeChars(String str)
  {
    closeTag();
    int len = str.length();
    for (int i = 0;  i < len;  i++)
      writeChar(str.charAt(i));
    prev = '-';
  }

  public void write(char[] buf, int off, int len)
  {
    closeTag();
    if (len <= 0)
      return;
    int limit = off + len;
    int count = 0;
    while (off < limit)
      {
	char c = buf[off++];
	if (c >= 127 || c == '\n' || c == '\r' || c == '<' || c == '>'
	    || c == '&' || (c == '"' && inAttribute))
	  {
	    if (count > 0)
	      super.write(buf, off - 1 - count, count);
	    writeChar(c);
	    count = 0;
	  }
	else
	  count++;
      }
    if (count > 0)
      super.write(buf, limit - count, count);
    prev = '-';
  }

  public boolean writePosition(AbstractSequence seq, int ipos)
  {
    seq.consumeNext(ipos, this);
    return true;
  }

  public void writeBaseUri (Object uri)
  {
  }

  public void writeComment(char[] chars, int offset, int length)
  {
    closeTag();
    print("<!--");
    super.write(chars, offset, length);
    print("-->");
    prev = '>';
  }

  public void writeCDATA (char[] chars, int offset, int length)
  {
    closeTag();
    print("<![CDATA[");
    int limit = offset+length;
    // Look for and deal with embedded "]]>".  This can't happen with
    // data generated from XML, but maybe somebody generated invalid CDATA.
    for (int i = offset;  i < limit - 2;  i++)
      {
	if (chars[i] == ']' && chars[i+1] == ']' && chars[i+2] == '>')
	  {
	    if (i > offset)
	      super.write(chars, offset, i - offset);
	    print("]]]><![CDATA[]>");
	    offset = i + 3;
	    length = limit - offset;
	    i = i + 2;
	  }
      }
    super.write(chars, offset, length);
    print("]]>");
    prev = '>';
  }

  public void writeProcessingInstruction(String target, char[] content,
					 int offset, int length)
  {
    if ("xml".equals(target))
      needXMLdecl = false;
    closeTag();
    print("<?");
    print(target);
    print(' ');
    write(content, offset, length);
    print("?>");
    prev = '>';
  }

  public boolean consume (SeqPosition position)
  {
    throw new Error("not implemented consume(TreePosition)");
  }
}

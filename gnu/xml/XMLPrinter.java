// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.*;
import gnu.text.Char;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter extends PrintConsumer
  implements PositionConsumer, XConsumer
{
  boolean inAttribute = false;
  boolean inStartTag = false;
  boolean needXMLdecl = false;
  boolean canonicalize = true;
  boolean htmlCompat = true;
  public boolean escapeText = true;
  boolean isHtml = false;
  boolean undeclareNamespaces = false;
  Object style;

  /** Chain of currently active namespace nodes. */
  NamespaceBinding namespaceBindings = NamespaceBinding.predefinedXML;

  /** Stack of namespaceBindings as of active beginGroup calls. */
  NamespaceBinding[] namespaceSaveStack = new NamespaceBinding[20];

  /** Difference between number of beginGroup and endGroup calls so far. */
  int groupNesting;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
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
    if (prev == WORD)
      {
	if (isWordChar((char) v))
	  super.write(' ');
      }
    // if (v >= 0x10000) emit surrogtes FIXME;
    if (! escapeText)
      super.write((char) v);
    else if (v == '<' && ! (isHtml && inAttribute))
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
      super.write((char) v);
    prev = v;
  }

  private void startWord()
  {
    closeTag();
    if (prev == WORD || isWordChar((char) prev))
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
	super.write('>');
	inStartTag = false;
	prev = '>';
      }
    else if (needXMLdecl)
      {
	// should also include encoding declaration FIXME.
	super.write("<?xml version=\"1.0\"?>\n");
	needXMLdecl = false;
      }
  }

  protected void startNumber()
  {
    startWord();
  }

  public void beginDocument()
  {
    // We should emit an XML declaration, but don't emit it set, in case
    // we get it later as a processing instruction.
    needXMLdecl = true;
  }

  public void beginGroup(String typeName, Object type)
  {
    closeTag();
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
	super.write(isHtml
		 ? (isHtmlEmptyElementTag(typeName) ? ">" : "></"+typeName+">")
		 : (htmlCompat ? " />" : "/>"));
	inStartTag = false;
      }
    else
      {
	super.write("</");
	super.write(typeName);
	super.write(">");
      }
    prev = '>';
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
    else if (v instanceof String || v instanceof CharSeq)
      {
	writeChars(v.toString());
      }
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
  }

  public void write(char[] buf, int off, int len)
  {
    closeTag();
    if (len <= 0)
      return;
    int ch = prev;
    char c;
    if (prev == WORD)
      {
	c = buf[off++];
	writeChar(c);
	ch = c;
	len--;
      }
    int limit = off + len;
    int count = 0;
    while (off < limit)
      {
	c = buf[off++];
	ch = c;
	if (ch >= 127 || ch == '<' || ch == '>'
	    || ch == '&' || (ch == '"' && inAttribute))
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

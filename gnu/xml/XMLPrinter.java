// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.PrintWriter;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter implements Consumer
{
  PrintWriter out;
  boolean inAttribute = false;
  boolean inStartTag = false;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  int prev = ' ';

  public XMLPrinter (PrintWriter out)
  {
    this.out = out;
  }

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  public void writeRaw(String str)
  {
    out.write(str);
  }

  public void writeChar(int v)
  {
    closeTag();
    if (prev == WORD)
      {
	if (isWordChar((char) v))
	  out.print(' ');
      }
    // if (v >= 0x10000) emit surrogtes FIXME;
    if (v == '<')
      out.print("&lt;");
    else if (v == '>')
      out.print("&gt;");
    else if (v == '&')
      out.print("&amp;");
    else if (v == '\"' && inAttribute)
      out.print("&quot;");
    else
      out.print((char) v);
    prev = v;
  }

  private void startWord()
  {
    closeTag();
    if (prev == WORD || isWordChar((char) prev))
      out.print(' ');
    prev = WORD;
  }

  public void writeBoolean(boolean v)
  {
    startWord();
    out.print(v);
  }

  public void writeFloat(float v)
  {
    startWord();
    out.print(v);
  }

  public void writeDouble(double v)
  {
    startWord();
    out.print(v);
  }

  public void writeInt(int v)
  {
    startWord();
    out.print(v);
  }

  private void closeTag()
  {
    if (inStartTag && ! inAttribute)
      {
	out.print('>');
	inStartTag = false;
	prev = '>';
      }
  }

  public void writeLong(long v)
  {
    startWord();
    out.print(v);
  }

  public void beginGroup(String typeName, Object type)
  {
    closeTag();
    out.print('<');
    out.print(typeName);
    inStartTag = true;
  }

  public void endGroup(String typeName)
  {
    if (inStartTag)
      {
	out.print("/>");
	inStartTag = false;
      }
    else
      {
	out.print("</");
	out.print(typeName);
	out.print('>');
      }
    prev = '>';
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(String attrName, Object attrType)
  {
    if (inAttribute)
      out.print('"');
    inAttribute = true;
    out.print(' ');
    out.print(attrName);
    out.print("=\"");
    prev = ' ';
  }

  public void endAttribute()
  {
    out.print('"');
    inAttribute = false;
    prev = ' ';
  }

  public void writeObject(Object v)
  {
    if (v instanceof Consumable)
      ((Consumable) v).consume(this);
    else
      {
	startWord();
	out.print(v);
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

  //public void writeChars(AbstractString str);

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
	if (ch >= 0x10000 || ch == '<' || ch == '>'
	    || ch == '&' || (ch == '"' && inAttribute))
	  {
	    if (count > 0)
	      out.write(buf, off - 1 - count, count);
	    writeChar(c);
	    count = 0;
	  }
	else
	  count++;
      }
    if (count > 0)
      out.write(buf, limit - count, count);
  }
}

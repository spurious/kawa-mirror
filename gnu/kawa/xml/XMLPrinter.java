// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.kawa.util.*;
import java.io.PrintWriter;

public class XMLPrinter implements Consumer
{
  PrintWriter out;
  boolean inAttribute = false;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  int prev;

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

  public void writeLong(long v)
  {
    startWord();
    out.print(v);
  }

  public void beginGroup(String typeName, Object type)
  {
    out.print('<');
    out.print(typeName);
  }

  public void endGroup(String typeName)
  {
    out.print("</");
    out.print(typeName);
    out.print('>');
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

  /** No more attributes in this group. */
  public void endAttributes()
  {
    if (inAttribute)
      out.print('"');
    inAttribute = false;
    out.print('>');
    prev = ' ';
  }

  public void writeObject(Object v)
  {
    startWord();
    out.print(v);
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
    int len = str.length();
    for (int i = 0;  i < len;  i++)
      writeChar(str.charAt(i));
  }

  //public void writeChars(AbstractString str);

  public void write(char[] buf, int off, int len)
  {
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
    while (len > 0)
      {
	int limit = off + len;
	int count = 0;

	for (;;)
	  {
	    if (off >= limit)
	      {
		out.write(buf, off - count, count);
		break;
	      }
	    c = buf[off++];
	    ch = c;
	    if (ch < 0x10000 && ch != '<' && ch != '>'
		&& ch != '&' && (ch != '"' || ! inAttribute))
	      count++;
	    else
	      {
		out.write(buf, off - 1 - count, count);
		writeChar(c);
		break;
	      }
	  }
	len -= count;
      }
    prev = ch;
  }
}

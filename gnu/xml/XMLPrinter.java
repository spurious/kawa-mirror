// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import java.io.PrintWriter;

/** Print an event stream in XML format on a PrintWriter. */

public class XMLPrinter implements Consumer, PositionConsumer
{
  Consumer out;
  boolean inAttribute = false;
  boolean inStartTag = false;
  boolean canonicalize = true;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  int prev = ' ';

  public XMLPrinter (Consumer out)
  {
    this.out = out;
  }

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  private final void writeRaw(String str)
  {
    out.writeChars(str);
  }

  public void writeChar(int v)
  {
    closeTag();
    if (prev == WORD)
      {
	if (isWordChar((char) v))
	  out.writeChar(' ');
      }
    // if (v >= 0x10000) emit surrogtes FIXME;
    if (v == '<')
      writeRaw("&lt;");
    else if (v == '>')
      writeRaw("&gt;");
    else if (v == '&')
      writeRaw("&amp;");
    else if (v == '\"' && inAttribute)
      writeRaw("&quot;");
    else
      out.writeChar((char) v);
    prev = v;
  }

  private void startWord()
  {
    closeTag();
    if (prev == WORD || isWordChar((char) prev))
      out.writeChar(' ');
    prev = WORD;
  }

  public void writeBoolean(boolean v)
  {
    startWord();
    out.writeBoolean(v);
  }

  public void writeFloat(float v)
  {
    startWord();
    out.writeFloat(v);
  }

  public void writeDouble(double v)
  {
    startWord();
    out.writeDouble(v);
  }

  public void writeInt(int v)
  {
    startWord();
    out.writeInt(v);
  }

  private void closeTag()
  {
    if (inStartTag && ! inAttribute)
      {
	out.writeChar('>');
	inStartTag = false;
	prev = '>';
      }
  }

  public void writeLong(long v)
  {
    startWord();
    out.writeLong(v);
  }

  public void beginGroup(String typeName, Object type)
  {
    closeTag();
    out.writeChar('<');
    writeRaw(typeName);
    inStartTag = true;
  }

  public void endGroup(String typeName)
  {
    if (canonicalize)
      closeTag();
    if (inStartTag)
      {
	writeRaw("/>");
	inStartTag = false;
      }
    else
      {
	writeRaw("</");
	writeRaw(typeName);
	writeRaw(">");
      }
    prev = '>';
  }

  /** Write a attribute for the current group.
   * This is only allowed immediately after a beginGroup. */
  public void beginAttribute(String attrName, Object attrType)
  {
    if (inAttribute)
      out.writeChar('"');
    inAttribute = true;
    out.writeChar(' ');
    writeRaw(attrName);
    writeRaw("=\"");
    prev = ' ';
  }

  public void endAttribute()
  {
    out.writeChar('"');
    inAttribute = false;
    prev = ' ';
  }

  public void writeObject(Object v)
  {
    if (v instanceof Consumable)
      ((Consumable) v).consume(this);
    else if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	pos.sequence.consumeNext(pos.ipos, pos.xpos, this);
      }
    else
      {
	startWord();
	out.writeChars(v.toString());
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

  public boolean writePosition(AbstractSequence seq, int ipos, Object xpos)
  {
    seq.consumeNext(ipos, xpos, this);
    return true;
  }

  public boolean consume(TreePosition position)
  {
    throw new Error("not implemented consume(TreePosition)");
  }
}

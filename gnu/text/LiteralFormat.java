package gnu.text;
import java.text.FieldPosition;
import java.io.Writer;

public class LiteralFormat extends ReportFormat
{
  char[] text;

  public LiteralFormat(char[] text)
  {
    this.text = text;
  }

  public LiteralFormat(String text)
  {
    this.text = text.toCharArray();
  }

  public LiteralFormat(StringBuffer sbuf)
  {
    int len = sbuf.length();
    text = new char[len];
    sbuf.getChars(0, len, text, 0);
  }

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)
    throws java.io.IOException
  {
    dst.write(text);
    return start;
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error("LiteralFormat.parseObject - not implemented");
  }

  /** Return the text that would be printed by the format. */
  public String content ()
  {
    return new String(text);
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer("LiteralFormat[\"");
    sbuf.append(text);
    sbuf.append("\"]");
    return sbuf.toString();
  }
}

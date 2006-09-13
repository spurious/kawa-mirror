package gnu.xquery.util;
import gnu.lists.*;
import gnu.text.Char;
import java.io.Writer;

/** A format that prints in XML syntax.
 * This may be a bad idea - maybe we should just use XMLConsumer.
 */

public class XMLFormat extends AbstractFormat
{
  boolean inAttribute = false;
  boolean inStartTag = false;

  /* If prev==WORD, last output was a number or similar. */
  private static final int WORD = -2;
  int prev;

  private void closeTag(Consumer out)
  {
    if (inStartTag && ! inAttribute)
      {
	write(">", out);
	inStartTag = false;
      }
  }

  protected static final boolean isWordChar(char ch)
  {
    return Character.isJavaIdentifierPart(ch) || ch == '-' || ch == '+';
  }

  private void startWord(Consumer out)
  {
    closeTag(out);
    if (prev == WORD || isWordChar((char) prev))
      write(" ", out);
    prev = WORD;
  }

  public void beginGroup(String typeName, Object type, Consumer out)
  {
    closeTag(out);
    write("<", out);
    write(typeName, out);
    inStartTag = true;
  }

  public void endGroup(String typeName, Consumer out)
  {
    if (inStartTag)
      {
	write("/>", out);
	inStartTag = false;
      }
    else
      {
	write("</", out);
	write(typeName, out);
	write(">", out);
      }
    prev = '>';
  }

  public void writeObject(Object obj, Consumer out)
  {
    closeTag(out);
    if (obj instanceof Boolean)
      writeBoolean(((Boolean)obj).booleanValue(), out);
    else if (obj instanceof Char)
      writeChar(((Char)obj).charValue(), out);
    else if (obj instanceof Character)
      writeChar(((Character)obj).charValue(), out);
    else if (obj instanceof Consumable)
      ((Consumable) obj).consume(out);
    else if (obj instanceof Consumable && out instanceof Writer)
      //((Printable) obj).print((PrintWriter) out);
      ((Consumable) out).consume(new gnu.xml.XMLPrinter((Writer) out));
    else if (obj == null)
      ;
    else
      {
	String str = obj.toString();
	if (str != null)
	  write (str, out);
      }
  }

  public void format (Object value, Consumer out)
  {
    Writer w = out instanceof Writer ? (Writer) out : new ConsumerWriter(out);
    gnu.xml.XMLPrinter xout = new gnu.xml.XMLPrinter(w);
    xout.writeObject(value);
  }
}

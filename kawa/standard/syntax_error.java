package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

/** Implements the Kawa extension "%syntax-error".
 * Prints out its arguments in an error message.
 * @author	Per Bothner
 */

public class syntax_error extends Syntax
{
  public Expression rewrite (Object obj, Translator tr)
  {
    StringBuffer buffer = new StringBuffer ();
    int words = 0;
    while (obj instanceof Pair)
      {
	Pair pair = (Pair) obj;
	if (words > 0)
	  buffer.append (' ');
	buffer.append (pair.car);
	obj = pair.cdr;
	words++;
      }
    if (obj != LList.Empty)
      {
	if (words > 0)
	  buffer.append (' ');
	buffer.append (obj);
      }
    return tr.syntaxError (buffer.toString ());
  }

  public static Expression error (Object form, Object[] message)
  {
    StringBuffer buffer = new StringBuffer();
    int len = message.length;
    if (message == null || len == 0)
      buffer.append("invalid syntax");
    else
      {
	for (int i = 0;  i < len;  i++)
	  buffer.append(message[i]);
      }
    Translator tr = (Translator) Compilation.getCurrent();
    if (tr == null)
      throw new RuntimeException(buffer.toString());
    Object savePos = tr.pushPositionOf(form);
    try
      {
	return tr.syntaxError(buffer.toString());
      }
    finally
      {
	tr.popPositionOf(savePos);
      }
  }
}

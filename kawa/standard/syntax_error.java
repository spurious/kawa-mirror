package kawa.standard;
import kawa.lang.*;

/** Implements the Kawa extension "%syntax-error".
 * Prints out its arguments in an error message.
 * @author	Per Bothner
 */

public class syntax_error extends Syntax
{
  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
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
    if (obj != List.Empty)
      {
	if (words > 0)
	  buffer.append (' ');
	buffer.append (obj);
      }
    return interp.syntaxError (buffer.toString ());
  }
}

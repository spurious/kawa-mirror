package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string->list".
 */

public class string2list extends Procedure1
{
  public string2list()
  {
    super("string->list");
  }

  public Object apply1 (Object arg1)
       throws WrongType
  {
    StringBuffer str = (StringBuffer) arg1;

    int len = str.length();

    List result = Interpreter.nullObject;
    for (int i=len; --i >= 0; )
      result = new Pair(Char.make (str.charAt(i)), result);
    return result;
  }
}

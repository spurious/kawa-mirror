package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string->list".
 */

public class string2list extends Procedure1
{
  public Object apply1 (Object arg1)
       throws WrongType
  {
    FString str = (FString) arg1;

    int len = str.length();

    List result = Interpreter.nullObject;
    for (int i=len; --i >= 0; )
      result = new Pair(Char.make (str.charAt(i)), result);
    return result;
  }
}

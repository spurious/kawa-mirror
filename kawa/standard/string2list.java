package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "string->list".
 */

public class string2list extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    FString str = (FString) arg1;

    int len = str.length();

    List result = List.Empty;
    for (int i=len; --i >= 0; )
      result = new Pair(Char.make (str.charAt(i)), result);
    return result;
  }
}

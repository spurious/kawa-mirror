package kawa.standard;
import gnu.kawa.util.*;
import gnu.text.Char;
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

    LList result = LList.Empty;
    for (int i=len; --i >= 0; )
      result = new Pair(Char.make (str.charAt(i)), result);
    return result;
  }
}

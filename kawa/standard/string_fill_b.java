package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string-fill!".
 */

public class string_fill_b extends Procedure2
{
  public Object apply2 (Object arg1,Object arg2)
  {
    FString str = (FString)arg1;
    char c = ((Char)arg2).charValue();

    int len = str.length();

    for (int t=0; t<len; t++)
      str.setCharAt(t,c);
    return Interpreter.voidObject;
  }
}

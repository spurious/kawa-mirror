package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string->list".
 */

public class string_fill_b extends Procedure2
{
  public string_fill_b()
  {
    super("string-fill!");
  }

  public Object apply2 (Object arg1,Object arg2)
  {
    StringBuffer str = (StringBuffer)arg1;
    char c = ((Char)arg2).charValue();

    int len = str.length();

    for (int t=0; t<len; t++)
      str.setCharAt(t,c);
    return str;
  }
}

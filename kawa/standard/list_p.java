package kawa.standard;
import gnu.kawa.util.LList;
import gnu.mapping.*;

/** Implement the standard Scheme function "list?". */

public class list_p extends Procedure1
{
  public Object apply1 (Object arg1)
  {
    if (LList.list_length (arg1) >= 0)
      return Boolean.TRUE;
    else
      return Boolean.FALSE;
  }
}

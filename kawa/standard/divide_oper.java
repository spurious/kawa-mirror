package kawa.standard;
import gnu.math.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "/".
 * @author Per Bothner
 */

public class divide_oper extends ProcedureN
{
  public Object applyN (Object[] args)
  {
    Numeric result;
    int i = 0;
    if (args.length == 1)
      result = IntNum.one ();
    else
      result = (Numeric) (args[i++]);
    for (; i < args.length;  i++)
      result = result.div (args[i]);
    return result;
   }
}

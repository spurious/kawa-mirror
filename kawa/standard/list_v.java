package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "list".
 * @author Per Bothner
 */

public class list_v extends ProcedureN
{
  public static Object list$V (Object[] args)
  {
    Object result = List.Empty;
    for (int i = args.length;  --i >= 0; )
      result = new Pair (args[i], result);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return list$V(args);
  }
}

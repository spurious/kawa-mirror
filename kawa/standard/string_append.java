package kawa.standard;
import gnu.kawa.util.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "string-append".
 * @author R. Alexander Milowski
 */

public class string_append extends ProcedureN
{
  public static FString stringAppend$V (Object[] args)
  {
    int count = args.length;
    java.lang.StringBuffer result = new java.lang.StringBuffer();

    for (int t=0; t<count; t++) {
       result.append(args[t].toString());
    }
    return new FString (result);
  }

  public Object applyN (Object[] args)
  {
    return stringAppend$V(args);
  }
}

package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string-append".
 * @author R. Alexander Milowski
 */

public class string_append extends ProcedureN
{
  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int count = args.length;
    java.lang.StringBuffer result = new java.lang.StringBuffer();

    for (int t=0; t<count; t++) {
       result.append(args[t].toString());
    }
    return new FString (result);
  }
}

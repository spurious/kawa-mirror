package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "string-append".
 * @author R. Alexander Milowski
 */

public class string_append extends ProcedureN
{
  public string_append()
  {
    super("string-append");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int count = args.length;
    if (count == 0) {
       throw new kawa.lang.WrongArguments(name,-1,"(string-append string ...)");
    }
    java.lang.StringBuffer result = new java.lang.StringBuffer();

    for (int t=0; t<count; t++) {
       result.append(((java.lang.StringBuffer)args[t]).toString());
    }
    return result;
  }
}

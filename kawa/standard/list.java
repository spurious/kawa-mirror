package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "list".
 * @author Per Bothner
 */

public class list extends ProcedureN
{
  public list()
  {
    super("list");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    Object result = Interpreter.nullObject;
    for (int i = args.length;  --i >= 0; )
      result = new Pair (args[i], result);
    return result;
  }
}

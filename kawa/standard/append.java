package kawa.standard;
import kawa.lang.*;

/**
 * Implement the Scheme standard function "append".
 * @author Per Bothner
 */

public class append extends ProcedureN
{
  public append()
  {
    super("append");
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    int count = args.length;
    if (count == 0)
      return List.Empty;
    Object result = args[count - 1];
    for (int i = count - 1; --i >= 0; )
      {
	Object list = args[i];
	Object copy = null;
	Pair last = null;
	while (list instanceof Pair)
	  {
	    Pair list_pair = (Pair) list;
	    Pair new_pair = new Pair (list_pair.car, null);
	    if (last == null)
	      copy = new_pair;
	    else
	      last.cdr = new_pair;
	    last = new_pair;
	    list = list_pair.cdr;
	  }
	if (list != List.Empty)
	  throw new WrongType(this.name (), 2, "list");
	if (last != null)
	  {
	    last.cdr = result;
	    result = copy;
	  }
      }
    return result;
  }
}

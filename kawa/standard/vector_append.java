package kawa.standard;
import gnu.lists.*;
import gnu.mapping.*;

/**
 * Implement the Scheme extended function "vector-append".
 * @author Per Bothner
 */

public class vector_append extends ProcedureN
{
  public static final vector_append vectorAppend
    = new vector_append("vector-append");

  public vector_append(String name)
  {
    super(name);
  }

  public Object applyN (Object[] args)
  {
    return apply$V(args);
  }

  public static FVector apply$V (Object[] args)
  {
    int length = 0;
    int args_length = args.length;
    for (int i = args_length;  --i >= 0; )
      {
	Object arg = args[i];
	if (arg instanceof FVector)
	  length += ((FVector)arg).size();
	else
	  {
	    int n = LList.listLength(arg, false);
	    if (n < 0)
	      throw new WrongType ("vector-append", i, "list or vector");
	    length += n;
	  }
      }
    Object[] result = new Object [length];
    int position = 0;
    for (int i = 0;  i < args_length;  i++)
      {
	Object arg = args[i];
	if (arg instanceof FVector)
	  {
	    FVector vec = (FVector) arg;
	    int vec_length = vec.size();
	    for (int j = 0;  j < vec_length;  j++)
	      result[position++] = vec.get(j);
	  }
	else if (arg instanceof Pair)
	  {
	    while (arg != LList.Empty)
	      {
		Pair pair = (Pair) arg;
		result[position++] = pair.car;
		arg = pair.cdr;
	      }
	  }
      }
    return new FVector (result);
  }
}

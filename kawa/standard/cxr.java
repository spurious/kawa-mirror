package kawa.standard;
import kawa.lang.*;

/** Implement the standard Scheme procedures "c[ad]*r". */

public class cxr extends Procedure1
{
  /** The number of car or cdr operations to apply. */
  int count;
  /** Whether an operation should be a car or a cdr.
   * The i'th operation is a cdr iff (mask >> i) & 1 is true. */
  int mask;

  /** Set count and mask from a function name of the form "c[ad]*r". */
  public void program (String name)
  {
    count = 0;
    mask = 0;
    int len = name.length ();
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt (i);
	if (ch == 'a' || ch == 'A')
	  {
	    mask <<= 1;
	    count++;
	  }
	else if (ch == 'd' || ch == 'D')
	  {
	    mask <<= 1;
	    mask |= 1;
	    count++;
	  }
      }
  }

  public Object apply1 (Object arg1)
       throws kawa.lang.WrongType,
	 kawa.lang.GenericError
  {
    if (count == 0)
      program (name().toString ());
    for (int i = count;  --count >= 0;  mask >>= 1)
      {
	if (! (arg1 instanceof Pair) )
	    throw new kawa.lang.WrongType(this.name (), 1, "list");
	Pair pair = (Pair) arg1;
	arg1 = (mask & 1) != 0 ? pair.cdr : pair.car;
      }
    return arg1;
  }
}

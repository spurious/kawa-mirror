package kawa.standard;
import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.HasSetter;
import gnu.mapping.WrongType;

/** Implement the standard Scheme procedures "c[ad]*r". */

public class cxr extends Procedure1 implements HasSetter
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
  {
    if (count == 0)
      program (name().toString ());
    int m = mask;
    for (int i = count;  --i >= 0;  m >>= 1)
      {
	if (! (arg1 instanceof Pair) )
	    throw new WrongType(this.name (), 1, "list");
	Pair pair = (Pair) arg1;
	arg1 = (m & 1) != 0 ? pair.cdr : pair.car;
      }
    return arg1;
  }

  public void set1 (Object value, Object list)
  {
    if (count == 0)
      program (name().toString ());
    int m = mask;
    Pair pair;
    for (int i = count;  --i > 0;  m >>= 1)
      {
	if (! (list instanceof Pair) )
	    throw new WrongType(this.name (), 1, "list");
	pair = (Pair) list;
	list = (m & 1) != 0 ? pair.cdr : pair.car;
      }
    pair = (Pair) list;
    if ((m & 1) != 0)
      pair.cdr = value;
    else
      pair.car = value;
  }
}

package kawa.standard;
import gnu.mapping.Procedure1;
import gnu.mapping.HasSetter;
import gnu.mapping.WrongType;
import gnu.lists.*;

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
    int c = 0;
    int m = 0;
    int len = name.length ();
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt (i);
	if (ch == 'a' || ch == 'A')
	  {
	    m <<= 1;
	    c++;
	  }
	else if (ch == 'd' || ch == 'D')
	  {
	    m <<= 1;
	    m |= 1;
	    c++;
	  }
      }
    mask = m;
    // Note we have to set count last, to avoid a race condition.
    count = c;
  }

  public Object apply1 (Object arg1)
  {
    if (count == 0)
      program(getName().toString());
    int m = mask;
    for (int i = count;  --i >= 0;  m >>= 1)
      {
	if (! (arg1 instanceof Pair) )
	    throw new WrongType(this.getName(), 1, "list");
	Pair pair = (Pair) arg1;
	arg1 = (m & 1) != 0 ? pair.cdr : pair.car;
      }
    return arg1;
  }

  public void set1 (Object list, Object value)
  {
    if (count == 0)
      program (getName().toString());
    int m = mask;
    Pair pair;
    for (int i = count;  --i > 0;  m >>= 1)
      {
	if (! (list instanceof Pair) )
	    throw new WrongType(this.getName(), 1, "list");
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

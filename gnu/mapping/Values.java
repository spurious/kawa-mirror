package gnu.mapping;

/** Encapsulate multiple values in a single object.
 * In Scheme and Lisp mainly used to return multiple values from a function.
 */

public class Values implements Printable
{
  private Object[] vals;

  public static final Values empty = new Values(Procedure.noArgs);

  /** Constructor.
   * @param values the values to encapulate
   */
  public Values (Object[] values)
  {
    vals = values;
  }

  /** Get the values encapsulated. */
  public Object[] values ()
  {
    return vals;
  }

  public static Object make (Object[] vals)
  {
    if (vals.length == 1)
      return vals[0];
    else if (vals.length == 0)
      return empty;
    else
      return new Values(vals);    
  }

  /** Apply a Procedure with these values as the arguments. */
  public Object call_with (Procedure proc)
  {
    return proc.applyN (vals);
  }

  public void print(java.io.PrintWriter ps)
  {
    if (this == empty)
      {
	ps.print("#!void");
	return;
      }
    int size = vals.length;
    ps.print("#<values");
    for (int i = 0; i < size; i++)
      {
	ps.print (" ");
	SFormat.print (vals[i], ps);
      }
    ps.print (">");
  }
}

package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import java.io.*;

public class ListRepeatPat extends Pattern implements Printable, Externalizable
{
  Pattern element_pattern;

  public ListRepeatPat ()
  {
  }

  public ListRepeatPat (Pattern element_pattern)
  {
    this.element_pattern = element_pattern;
  }

  public static ListRepeatPat make (Pattern element_pattern)
  {
    return new ListRepeatPat (element_pattern);
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<list-repeat-pattern ");
    SFormat.print (element_pattern, ps);
    ps.print ('>');
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    /* DEBUGGING
      System.err.print ("(match ");
      print (System.err);
      System.err.print (" on ");
      SFormat.print (obj, System.err);
      System.err.print (")\n");
      */
    int length = LList.list_length (obj);
    if (length < 0)
      return false;

    int var_count = element_pattern.varCount ();
    for (int i = var_count;  --i >= 0; )
      vars[start_vars + i] = new Object [length];
    Object[] element_vars = new Object [var_count];
    for (int j = 0; j < length;  j++)
      {
	Pair pair = (Pair) obj;
	/* DEBUGGING
	   System.err.print ("(sub-match ["+j+"] ");
	   SFormat.print (element_pattern, System.err);
	   System.err.print (" on ");
	   SFormat.print (pair.car, System.err);
	   */

	if (! element_pattern.match (pair.car, element_vars, 0))
	  return false;
	for (int i = 0;  i < var_count;  i++)
	  ((Object[]) vars[start_vars + i]) [j] = element_vars[i];
	obj = pair.cdr;
      }
    return true;
  }

  public int varCount () { return element_pattern.varCount (); }

  /**
   * @serialData Write the element_pattern (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(element_pattern);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    element_pattern = (Pattern) in.readObject();
  }
}

package gnu.mapping;
import java.io.*;
import gnu.lists.*;

/** Encapsulate multiple values in a single object.
 * In Scheme and Lisp mainly used to return multiple values from a function.
 */

public class Values extends TreeList implements Printable, Externalizable
{
  public static final Object[] noArgs = new Object[0];

  public static final Values empty = new Values(noArgs);

  public Values ()
  {
  }

  /** Constructor.
   * @param values the values to encapulate
   */
  public Values (Object[] values)
  {
    for (int i = 0;  i < values.length;  i++)
      writeObject(values[i]);
  }

  /** Get the values encapsulated. */
  // Used by CallContext.writeValue, call_with_values.apply(CallContext) FIXME
  public Object[] getValues ()
  {
    return isEmpty() ? noArgs : toArray();
  }

  public static Object values$V(Object[] vals)
  {
    return make(vals);
  }

  public static Values make ()
  {
    return new Values();
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

  public static Object make (Sequence seq)
  {
    int count = seq.size();
    if (count == 0)
      return empty;
    if (count == 1)
      return seq.get(0);
    Values vals = new Values();
    java.util.Enumeration it = seq.elements();
    while (it.hasMoreElements())
      vals.writeObject(it.nextElement());
    return vals;
  }

  public static Object make (TreeList list)
  {
    return make(list, 0, list.data.length);
  }

  public static Object make (TreeList list, int startPosition, int endPosition)
  {
    int next;
    if (startPosition == endPosition
	|| (next = list.nextDataIndex(startPosition)) <= 0)
      return empty;
    if (next == endPosition || list.nextDataIndex(next) < 0)
      return list.getNext(startPosition << 1, null); // Singleton value
    Values vals = new Values();
    list.consumeRange(startPosition, endPosition, vals);
    return vals;
  }

  /** Apply a Procedure with these values as the arguments. */
  public Object call_with (Procedure proc) throws Throwable
  {
    return proc.applyN (toArray());
  }

  public boolean equals (Object obj)
  {
    if (obj instanceof Values)
      return super.equals(obj);
    if (size() != 0)
      return false;
    Object x = getNext(0, null);
    return x != null && x.equals(obj);
  }

  public void print(java.io.PrintWriter ps)
  {
    if (this == empty)
      {
	ps.print("#!void");
	return;
      }
    Object[] vals = toArray();  // FIXME!
    int size = vals.length;
    ps.print("#<values");
    for (int i = 0; i < size; i++)
      {
	ps.print (" ");
	SFormat.print (vals[i], ps);
      }
    ps.print (">");
  }

  /**
   * @serialData Write the length (using writeInt), followed by
   *   the values in order (written using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    Object[] vals = toArray();  // FIXME
    int len = vals.length;
    out.writeInt(len);
    for (int i = 0;  i < len;  i++)
      out.writeObject(vals[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int len = in.readInt();
    for (int i = 0;  i < len;  i++)
      writeObject(in.readObject());
  }

  public Object readResolve() throws ObjectStreamException
  {
    return isEmpty() ? empty : this;
  }

  /** Helper method called by code using a SeriesTarget.
   * The compiled code iterates through zero or more values.
   * Return the index of the next value, or -1 if currently at eof.
   * A non-Values object is treated as a singleton value,
   * so in that case there is no next value.
   */
  public static int nextIndex(Object values, int curIndex)
  {
    if (values instanceof Values)
      return ((Values) values).nextDataIndex(curIndex);
    else
      return curIndex == 0 ? 1 : -1;
  }

  /** Helper method called by code using a SeriesTarget.
   * The compiled code iterates through zero or more values.
   * Extract the object referenced by the curIndex.
   * A non-Values object is treated as a singleton value.
   */
  public static Object nextValue(Object values, int curIndex)
  {
    if (values instanceof Values)
      return ((Values) values).getNext(curIndex << 1, null);
    else
      return values;
  }

  public static void writeValues(Object value, Consumer out)
  {
    if (value instanceof Values)
      {
	((Values) value).consume(out);
	/*
	Object[] values = ((Values) value).getValues();
	for (int i = 0;  i < values.length;  i++)
	  writeValues(values[i], out);
	*/
      }
    /*
    else if (value instanceof Consumable)
      {
	((Consumable) value).consume(out);
      }
    */
    else
      out.writeObject(value);
  }
}

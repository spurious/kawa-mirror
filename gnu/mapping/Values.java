package gnu.mapping;
import java.io.*;
import gnu.lists.*;
import gnu.text.Printable;
import java.util.*;

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

  /* #ifdef JAVA5 */
  public static Object values(Object... vals)
  /* #else */
  // public static Object values$V(Object[] vals)
  /* #endif */
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

  public static Object make
  /* #ifdef JAVA2 */
  (List seq)
  /* #else */
  // (Sequence seq)
  /* #endif */
  {
    int count = seq == null ? 0 : seq.size();
    if (count == 0)
      return empty;
    if (count == 1)
      return seq.get(0);
    Values vals = new Values();
    /* #ifdef JAVA2 */
    java.util.Iterator it = seq.iterator();
    while (it.hasNext())
      vals.writeObject(it.next());
    /* #else */
    // java.util.Enumeration it = seq.elements();
    // while (it.hasMoreElements())
    //   vals.writeObject(it.nextElement());
    /* #endif */
    return vals;
  }

  public static Object make (TreeList list)
  {
    return make(list, 0, list.data.length);
  }

  /**
   * Extract a value (single or Values) from a sub-range of a TreeList.
   * @param list the TreeList to copy
   * @param startPosition start of range, as a raw index in data
   * @param endPosition end of range, as a raw index in data
   */
  public static Object make (TreeList list, int startPosition, int endPosition)
  {
    int next;
    if (startPosition == endPosition
	|| (next = list.nextDataIndex(startPosition)) <= 0)
      return empty;
    if (next == endPosition || list.nextDataIndex(next) < 0)
      return list.getPosNext(startPosition << 1); // Singleton value
    Values vals = new Values();
    list.consumeIRange(startPosition, endPosition, vals);
    return vals;
  }

  /** If a simple value, return that value.
   * Also, if no values, return empty.
   */
  public final Object canonicalize ()
  {
    if (gapEnd == data.length)
      {
	if (gapStart == 0)
	  return empty;
	if (nextDataIndex(0) == gapStart) // Singleton value.
	  return getPosNext(0);
      }
    return this;
  }

  /** Apply a Procedure with these values as the arguments. */
  public Object call_with (Procedure proc) throws Throwable
  {
    return proc.applyN (toArray());
  }

  public void print (Consumer out)
  {
    if (this == empty)
      {
	out.write("#!void");
	return;
      }
    Object[] vals = toArray();  // FIXME!
    int size = vals.length;
    boolean readable = true;  // FIXME
    if (readable)
      out.write("#<values");
    for (int i = 0;;)
      {
        int next = nextDataIndex(i);
        if (next < 0)
          break;
	out.write(' ');
        if (i >= gapEnd)
          i -= gapEnd - gapStart;
        Object val = getPosNext(i << 1);
        if (val instanceof Printable)
          ((Printable) val).print(out);
        else
          out.writeObject(val);
        i = next;
      }
    if (readable)
      out.write('>');
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

  /** Helper method called by compiled code.
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

  /** Helper method called by compiled code.
   * The compiled code iterates through zero or more values.
   * Extract the object referenced by the curIndex.
   * A non-Values object is treated as a singleton value.
   */
  public static Object nextValue(Object values, int curIndex)
  {
    if (values instanceof Values)
      {
	Values v = (Values) values;
	if (curIndex >= v.gapEnd)
	  curIndex -= v.gapEnd - v.gapStart;
	return ((Values) values).getPosNext(curIndex << 1);
      }
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

  public static int countValues (Object value)
  {
    return value instanceof Values ? ((Values) value).size() : 1;
  }
}

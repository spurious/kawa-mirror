// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.PrintWriter;
import java.io.*;

/**
 * Semi-abstract class for traditions Lisp-style lists.
 * A list is implemented as a chain of Pair objects, where the
 * 'car' field of the Pair points to a data element, and the 'cdr'
 * field points to the next Pair.  (The names 'car' and 'cdr' are
 * historical; they refer to hardware on machines form the 60's.)
 * Includes singleton static Empty, and the Pair sub-class.
 * @author	Per Bothner
 */

public class LList extends AbstractSequence implements Sequence, Externalizable
{
  /** Do not use - only public for serialization! */
  public LList () { }

  static public final LList Empty = new LList ();

  /**
   * A safe function to count the length of a list.
   * @param obj the putative list to measure
   * @param allowOtherSequence if a non-List Sequence is seen, allow that
   * @return the length, or -1 for a circular list, or -2 for an improper list
   */
  static public int listLength(Object obj, boolean allowOtherSequence)
  {
    // Based on list-length implementation in
    // Guy L Steele jr: "Common Lisp:  The Language", 2nd edition, page 414
    int n = 0;
    Object slow = obj;
    Object fast = obj;
    for (;;)
      {
	if (fast == Empty)
	  return n;
	if (! (fast instanceof Pair))
	  {
	    if (fast instanceof Sequence && allowOtherSequence)
	      {
		int j = ((Sequence) fast).size ();
		return j >= 0 ? n + j : j;
	      }
	    return -2;
	  }
	Pair fast_pair = (Pair) fast;
	if (fast_pair.cdr == Empty)
	  return n+1;
	if (fast == slow && n > 0)
	  return -1;
	if (! (fast_pair.cdr instanceof Pair))
	  {
	    n++;
	    fast = fast_pair.cdr;
	    continue;
	  }
	if (!(slow instanceof Pair))
	  return -2;
	slow = ((Pair)slow).cdr;
	fast = ((Pair)fast_pair.cdr).cdr;
	n += 2;
      }
  }

  public boolean equals (Object obj)
  {
    // Over-ridden in Pair!
    return this == obj;
  }

  public int size()
  {
    // Over-ridden in Pair!
    return 0;
  }

  public boolean isEmpty()
  {
    // Over-ridden in Pair!
    return true;
  }

  // Positions are represented in a non-obvious way.  After a next(),
  // xpos points to *two* Pairs before the logical iterator position
  // (the cursor).  This is because of the requirement for remove(),
  // which needs to be able to remove the element *before* the cursor.
  // But to unlink that element, we need to change the 'next' field
  // of the Pair before that again.
  // However, after a call to 'previous', we cannot cheaply move the
  // xpos pointer backwards, so we leave it as is.  Therefore, we
  // get the rule that when isAfter() is false the cursor position is
  // after the Pair pointed by xpos; when isAfter() is true then
  // the cursor position is after the Pair ((Pair) xpos).cdr).
  // If the cursor position is too early in the list to satisfy this
  // invariant, then xpos==null.
  // ??? Can we handle improper lists (whose tail is a sequence)? FIXME

  public void makePosition(int index, boolean isAfter,
			   PositionContainer poses, int positionNumber)
  {
    int ipos = (index << 1) | (isAfter ? 1 : 0);
    Object p = this;
    int skip = index;
    if (isAfter)
      {
        skip -= 2;
      }
    else
      {
        skip -= 1;
      }
    if (skip < 0)
      p = null;
    else
      {
        while (--skip >= 0)
          {
            p = ((Pair) p).cdr;
          }
      }
    poses.setPosition(positionNumber, ipos, p);
    poses.setSequence(positionNumber, this);
  }

  protected int nextIndex(int ipos, Object xpos)
  {
    return ipos >>> 1;
  }

  public boolean hasNext(int ipos, Object xpos)
  {
    // Equivalent to getNext(ipos, xpos) != eofValue.
    Object next;
    if (xpos == null)
      {
        if ((ipos >> 1) == 0)
          return this != Empty;
        return ((Pair) this).cdr != Empty;
      }
    else
      next = ((Pair) xpos).cdr;
    if ((ipos & 1) > 0) // if isAfter
      next = ((Pair) next).cdr;
    return next != Empty;
  }

  public Object getNext(int ipos, Object xpos)
  {
    int isAfter = (ipos & 1);
    Object next;
    if (isAfter > 0)
      {
        if (xpos == null)
          {
	    next = this;
	    if ((ipos >> 1) != 0)
	      next = ((Pair) next).cdr;
          }
        else
          next = ((Pair) (((Pair) xpos).cdr)).cdr;
      }
    else
      {
        if (xpos == null)
          next = this;
        else
          next = xpos;
      }
    if (next == Empty)
      return eofValue;
    return ((Pair) next).car;
  }

  public Object get (int index)
  {
    throw new ArrayIndexOutOfBoundsException (index);
  }
  
  /* Count the length of a list.
   * Note: does not catch circular lists!
   * @param arg the list to count
   * @return the length
   */
  static public final int length (Object arg)
  {
    int count = 0;
    for ( ; arg instanceof Pair; arg = ((Pair)arg).cdr)
      count++;
    return count;
  }

  protected boolean isAfter(int ipos, Object xpos)
  {
    return (ipos & 1) != 0;
  }

  public boolean gotoNext(PositionContainer posSet, int posNumber)
  {
    int ipos = posSet.getPositionInt(posNumber);
    Object xpos = posSet.getPositionPtr(posNumber);
    boolean isAfter = (ipos & 1) != 0;
    if (xpos != null)
      {
	if (isAfter)
	  xpos = ((Pair) xpos).cdr;
	if (((Pair) xpos).cdr == Empty)
	  return false;
	ipos = (ipos | 1) + 2;
      }
    else if ((ipos >> 1) == 0) // position is 0
      {
	if (this == Empty)
	  return false;
	ipos = (1 << 1) | 1;
      }
    else // position is 1, iAfter must be true
      {
	xpos = this;
	if (((Pair) xpos).cdr == Empty)
	  return false;
	ipos = (2 << 1) | 1;
      }
    posSet.setPosition(posNumber, ipos, xpos);
    return true;
  }
  
  public static LList makeList (Sequence vals)
  {
    java.util.Enumeration e = ((AbstractSequence) vals).elements();
    LList result = LList.Empty;
    Pair last = null;
    for (int i = 0;  e.hasMoreElements(); i++)
      {
        Pair pair = new Pair(e.nextElement(), LList.Empty);
        if (last == null)
          result = pair;
        else
          last.cdr = pair;
        last = pair;
      }
    return result;
  } 

  public static LList makeList (Object[] vals, int offset, int length)
  {
    LList result = LList.Empty;
    for (int i = length;  --i >= 0; )
      result = new Pair (vals[offset+i], result);
    return result;
  }

  public static LList makeList (Object[] vals, int offset)
  {
    /* DEBUGGING:
    for (int i = 0;  i < vals.length; i++)
      {
	if (i > 0)
	  System.err.print(", ");
	System.err.println(vals[i]);
      }
    System.err.println("], "+offset+")");
    */
    LList result = LList.Empty;
    for (int i = vals.length - offset;  --i >= 0; )
      result = new Pair (vals[offset+i], result);
    return result;
  }

  /* FIXME
  public FVector toVector ()
  {
    int len = size();

    Object[] values = new Object[len];
    Object list = this;
    for (int i=0; i < len; i++)
      {
	Pair pair = (Pair) list;
	values[i] = pair.car;
	list = pair.cdr;
      }
    return new FVector (values);
  }
  */

  public void consume(Consumer out)
  {
    Object list = this;
    String typeName = "list";
    String type = typeName;
    out.beginGroup(typeName, type);
    while (list instanceof Pair)
      {
	if (list != this)
	  out.writeChar(' ');
	Pair pair = (Pair) list;
	out.writeObject(pair.car);
	list = pair.cdr;
      }
    if (list != Empty)
      {
	out.writeChar(' ');
	out.writeChars(". ");
	out.writeObject(list);
      }
    out.endGroup(typeName);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
  }

  /**
   * @serialData Write nothing.
   *  (Don't need to write anything.)
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
  }

  public Object readResolve() throws ObjectStreamException
  {
    return Empty;
  }

  public static Pair list1(Object arg1)
  {
    return new Pair(arg1, LList.Empty);
  }

  public static Pair list2(Object arg1, Object arg2)
  {
    return new Pair(arg1, new Pair(arg2, LList.Empty));
  }

  public static Pair list3(Object arg1, Object arg2, Object arg3)
  {
    return new Pair(arg1, new Pair(arg2, new Pair(arg3, LList.Empty)));
  }

  public static Pair list4(Object arg1, Object arg2, Object arg3, Object arg4)
  {
    return new Pair(arg1, new Pair(arg2,
				   new Pair(arg3, new Pair (arg4,
							    LList.Empty))));
  }

  /** Utility function used by compiler when inlining `list'. */
  public static Pair chain1 (Pair old, Object arg1)
  {
    Pair p1 = new Pair(arg1, LList.Empty);
    old.cdr = p1;
    return p1;
  }

  /** Utility function used by compiler when inlining `list'. */
  public static Pair chain4 (Pair old, Object arg1, Object arg2,
		      Object arg3, Object arg4)
  {
    Pair p4 = new Pair(arg4, LList.Empty);
    old.cdr = new Pair(arg1, new Pair(arg2, new Pair(arg3, p4)));
    return p4;
  }

  /** Reverse a list in place, by modifying the cdr fields. */
  public static LList reverseInPlace(Object list)
  {
    // Algorithm takes from reverse function in gcc's tree.c.
    LList prev = Empty;
    while (list != Empty)
      {
	Pair pair = (Pair) list;
	list = pair.cdr;
	pair.cdr = prev;
	prev = pair;
      }
    return prev;
  }

  public static Object listTail(Object list, int count)
  {
    while (--count >= 0)
      {
	if (! (list instanceof Pair))
	  throw new IndexOutOfBoundsException("List is too short.");
	Pair pair = (Pair) list;
	list = pair.cdr;
      }
    return list;
  }
}

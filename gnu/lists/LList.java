// FIX BRL's use to toString

// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
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

public class LList extends ExtSequence implements Sequence, Externalizable
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

  public SeqPosition getIterator()
  {
    return new LListPosition(this, 0, false);
  }

  public int createPos(int index, boolean isAfter)
  {
    ExtPosition pos = new LListPosition(this, index, isAfter);
    return PositionManager.manager.register(pos);
  }

  public int createRelativePos(int pos, int delta, boolean isAfter)
  {
    boolean old_after = isAfterPos(pos);
    if (delta < 0 || pos == 0)
      return super.createRelativePos(pos, delta, isAfter);
    if (delta == 0)
      {
	if (isAfter == old_after)
	  return copyPos(pos);
	if (isAfter && ! old_after)
	  return super.createRelativePos(pos, delta, isAfter);
      }
    if (pos < 0)
      throw new IndexOutOfBoundsException();
    LListPosition old = (LListPosition) PositionManager.getPositionObject(pos);
    if (old.xpos == null)
      return super.createRelativePos(pos, delta, isAfter);
    LListPosition it = new LListPosition(old);
    Object it_xpos = it.xpos;
    int it_ipos = it.ipos;
    if (isAfter && ! old_after)
      {
	delta--;
	it_ipos += 3;
      }
    if (! isAfter && old_after)
      {
	delta++;
	it_ipos -= 3;
      }
    for (;;)
      {
	if (! (it_xpos instanceof Pair))
	  throw new IndexOutOfBoundsException();
	if (--delta < 0)
	  break;
	Pair p = (Pair) it_xpos;
	it_ipos += 2;
	it_xpos = p.cdr;
      }
    it.ipos = it_ipos;
    it.xpos = it_xpos;
    return PositionManager.manager.register(it);
  }

  public boolean hasNext (int ipos)
  {
    return false;  // Overridden in Pair.
  }

  public int nextPos (int ipos)
  {
    return 0;  // Overridden in Pair.
  }

  public Object getPosNext (int ipos)
  {
    return eofValue;  // Overridden in Pair.
  }

  public Object getPosPrevious (int ipos)
  {
    return eofValue;  // Overridden in Pair.
  }

  protected void setPosNext(int ipos, Object value)
  {
    if (ipos <= 0)
      {
	if (ipos == -1 || ! (this instanceof Pair))
	  throw new IndexOutOfBoundsException();
	((Pair) this).car = value;
      }
    else
      PositionManager.getPositionObject(ipos).setNext(value);
  }

  protected void setPosPrevious(int ipos, Object value)
  {
    if (ipos <= 0)
      {
	if (ipos == 0 || ! (this instanceof Pair))
	  throw new IndexOutOfBoundsException();
	((Pair) this).lastPair().car = value;
      }
    else
      PositionManager.getPositionObject(ipos).setPrevious(value);
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

  public String toString ()
  {
    Object rest = this;
    int i = 0;
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append('(');
    for (;;)
      {
	if (rest == Empty)
	  break;
	if (i > 0)
	  sbuf.append(' ');
	if (i >= 10)
	  {
	    sbuf.append("...");
	    break;
	  }
	if (rest instanceof Pair)
	  {
	    Pair pair = (Pair) rest;
	    sbuf.append(pair.car);
	    rest = pair.cdr;
	  }
	else
	  {
	    sbuf.append(". ");
	    sbuf.append(rest);
	    break;
	  }
	i++;
      }
    sbuf.append(')');
    return sbuf.toString();
  }
}

/** LListPosition - a SeqPosition for LLists.
 * Normally, we just use a since int Pos to indicate a position in a
 * sequence.  But for linked lists that would be very inefficient.
 * So this sub-class in addition uses an xpos field to point to the
 * current Pair.  However, we do so in a non-obvious way.  After a next(),
 * xpos points to *two* Pairs before the logical iterator position
 * (the cursor).  This is because of the requirement for remove(),
 * which needs to be able to remove the element *before* the cursor.
 * But to unlink that element, we need to change the 'next' field
 * of the Pair before that again.
 * However, after a call to 'previous', we cannot cheaply move the
 * xpos pointer backwards, so we leave it as is.  Therefore, we
 * get the rule that when isAfter() is false the cursor position is
 * after the Pair pointed by xpos; when isAfter() is true then
 * the cursor position is after the Pair ((Pair) xpos).cdr).
 * If the cursor position is too early in the list to satisfy this
 * invariant, then xpos==null.
 */

class LListPosition extends ExtPosition
{
  /**
   * An Object that (together with ipos) indicates the current position.
   * The actual value has no meaning, except as interpreted by sequence.
   */
  Object xpos;

  public LListPosition (LListPosition old)
  {
    sequence = old.sequence;
    ipos = old.ipos;
    xpos = old.xpos;
  }

  public SeqPosition copy ()
  {
    return new LListPosition(this);
  }

  public LListPosition (LList seq, int index, boolean isAfter)
  {
    set(seq, index, isAfter);
  }

  public void set (LList seq, int index, boolean isAfter)
  {
    sequence = seq;
    ipos = (index << 1) | (isAfter ? 1 : 0);
    int skip = index;
    if (isAfter)
      {
        skip -= 2;
      }
    else
      {
        skip -= 1;
      }
    if (skip >= 0)
      {
	Object p = seq;
        while (--skip >= 0)
          {
            p = ((Pair) p).cdr;
          }
	xpos = p;
      }
  }

  public boolean hasNext()
  {
    Object next;
    if (xpos == null)
      {
        if ((ipos >> 1) == 0)
          return sequence != LList.Empty;
        return ((Pair) sequence).cdr != LList.Empty;
      }
    else
      next = ((Pair) xpos).cdr;
    if ((ipos & 1) > 0) // if isAfter
      next = ((Pair) next).cdr;
    return next != LList.Empty;
  }

  /** Get the Pair whose car contains the next element, or null. */
  public Pair getNextPair ()
  {
    int isAfter = (ipos & 1);
    Object next;
    if (isAfter > 0)
      {
        if (xpos == null)
          {
	    next = sequence;
	    if ((ipos >> 1) != 0)
	      next = ((Pair) next).cdr;
          }
        else
          next = ((Pair) (((Pair) xpos).cdr)).cdr;
      }
    else
      {
        if (xpos == null)
          next = sequence;
        else
          next = xpos;
      }
    if (next == LList.Empty)
      return null;
    return (Pair) next;
  }

  public Object getNext ()
  {
    Pair pair = getNextPair();
    return pair == null ? LList.eofValue : pair.car;
  }

  public void setNext (Object value)
  {
    Pair pair = getNextPair();
    pair.car = value;
  }

  /** Get the Pair whose car contains the previous element, or null. */
  public Pair getPreviousPair ()
  {
    int isAfter = (ipos & 1);
    Object p = xpos;
    if (isAfter > 0)
      p = p == null ? sequence : ((Pair) p).cdr;
    else if (p == null)
      return null;
    if (p == LList.Empty)
      return null;
    return (Pair) p;
  }

  public Object getPrevious ()
  {
    Pair pair = getPreviousPair();
    return pair == null ? LList.eofValue : pair.car;
  }

  public void setPrevious (Object value)
  {
    Pair pair = getPreviousPair();
    pair.car = value;
  }

  public int nextIndex ()
  {
    return ipos >> 1;
  }

  public boolean gotoNext()
  {
    boolean isAfter = (ipos & 1) != 0;
    int old_i = ipos;
    Object xp = xpos;
    if (xp != null)
      {
	if (isAfter)
	  xp = ((Pair) xp).cdr;
	if (((Pair) xp).cdr == LList.Empty)
	  return false;
	xpos = xp;
	ipos = (ipos | 1) + 2;
      }
    else if ((ipos >> 1) == 0) // position is 0
      {
	if (sequence == LList.Empty)
	  return false;
	ipos = (1 << 1) | 1;
      }
    else // position is 1, iAfter must be true
      {
	xp = sequence;
	if (((Pair) xp).cdr == LList.Empty)
	  return false;
	ipos = (2 << 1) | 1;
	xpos = xp;
      }
    return true;
  }

  public boolean gotoPrevious()
  {
    if (ipos >>> 1 == 0)
      return false;
    if ((ipos & 1) != 0) // isAfter()
      {
	// Decrement index; clear isAfter bit.
	ipos -= 3;
	return true;
      }
    int index = nextIndex();
    set(sequence, index - 1, false);
    return true;
  }

  public String toString()
  {
    return "LListPos["+super.toString()+" pos:"+position+']';
  }
}

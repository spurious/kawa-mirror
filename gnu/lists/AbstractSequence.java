// Copyright (c) 2001, 2002, 2003, 2005  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

import gnu.kawa.util.HashUtils;
import java.util.*;

/**
 * A collection of default methods for implementing sequence-like classes.
 *
 * Additionally, a sequence may have zero or more attributes, which are
 * name-value pairs.  A sequence may also have a named "type".  These
 * extensions are to support XML functionality - it might be cleaner to
 * move them to a sub-class of Sequence or some interface.
 *
 * Many of the protected methods in Sequence (such as nextIndex) are
 * only intended to be called from SeqPosition or TreePosition, see those.
 *
 * @author Per Bothner
 */

public abstract class AbstractSequence<E>
{
    public static final int[] noInts = new int[0];

    public int size() {
        if (rank() == 1) return getSize(0);
        throw unsupported("size");
    }

    public int getSize() {
        int sz = 1;
        for (int r = rank(); --r >= 0; )
            sz *= getSize(r);
        return sz;
    }

  public boolean isEmpty()
  {
    return size() == 0;
  }

  public int rank()
  {
    return 1;
  }

    protected void checkRank(int i) {
        if (i != rank())
            throw badRank(i);
    }

    protected RuntimeException badRank(int i) {
        return new RuntimeException("wrong number of indexes "+i+" to "+rank()+"-rank array");
    }

    // FIXME remove this - some implementations needed
    public Array<E> asImmutable() { throw unsupported("asImmutable"); }

    public E get() { return getRaw(effectiveIndex()); }
    public E get(int i) {
        return getRaw(effectiveIndex(i));
    }
    public E get(int i, int j) { return getRaw(effectiveIndex(i,j)); }
    public E get(int i, int j, int k, int... rest) {
        return getRaw(effectiveIndex(i, j, k, rest)); } 

    public E get(int[] indexes) {
        return getRaw(effectiveIndex(indexes));
    }

    protected void checkCanWrite () {
    }
    public E getRowMajor(int index) {
        if (rank() == 1)
            return get(index);
        if (this instanceof Array)
            return Arrays.getRowMajor((Array<E>) this, index);
        throw unsupportedException("getRowMajor");
    }

    public int effectiveIndex() {
        checkRank(0);
        return 0;
    }
    public int effectiveIndex(int index) {
        checkRank(1);
        return index - getLowBound(0);
    }
    public int effectiveIndex(int i, int j) {
        checkRank(2);
        return (i - getLowBound(0)) * getSize(1) + (j - getLowBound(1));
    }
    public int effectiveIndex(int i, int j, int k, int... rest) {
        int r = rest.length;
        checkRank(3+r);
        int eff = 0;
        int stride = 1;
        while (--r >= 0) {
            eff += (rest[r] - getLowBound(3+r)) * stride;
            stride *= getSize(3+r);
        }
        eff += (k - getLowBound(2)) * stride;
        stride *= getSize(2);
        eff += (j - getLowBound(1)) * stride;
        stride *= getSize(1);
        eff += (i - getLowBound(0)) * stride;
        return eff;
    }

    public int effectiveIndex(int[] indexes) {
        int ilength = indexes.length;
        switch (indexes.length) {
        case 0:
            return effectiveIndex();
        case 1:
            return effectiveIndex(indexes[0]);
        case 2:
            return effectiveIndex(indexes[0], indexes[1]);
        default:
            int[] rest;
            if (ilength == 3)
                rest = noInts;
            else {
                rest = new int[ilength-3];
                System.arraycopy(indexes, 3, rest, 0, ilength-3);
            }
            return effectiveIndex(indexes[0], indexes[1], indexes[2], rest);
        }
    }

    public E getRaw(int index) { throw unsupported("getRaw"); }

    protected void setBuffer(Object obj) { throw unsupported("setBuffer"); }

    /** Given an "effective index", set selected element. */
    public void setRaw(int index, E value) { throw unsupported("setRaw"); }

    public boolean getBooleanRaw(int index) {
        Object value = getRaw(index);
        return value != null && ((Boolean) value).booleanValue();
    }

    public char getCharRaw(int index) {
        return ((Character) getRaw(index)).charValue();
    }

    public byte getByteRaw(int index) {
        return ((Number) getRaw(index)).byteValue();
    }

    public short getShortRaw(int index) {
        return ((Number) getRaw(index)).shortValue();
    }

    public int getIntRaw(int index) {
        return ((Number) getRaw(index)).intValue();
    }

    public long getLongRaw(int index) {
        return ((Number) getRaw(index)).longValue();
    }

    public float getFloatRaw(int index) {
        return ((Number) getRaw(index)).floatValue();
    }

    public double getDoubleRaw(int index) {
        return ((Number) getRaw(index)).doubleValue();
    }

    public int getInt() { return getIntRaw(effectiveIndex()); }
    public int getInt(int i) { return getIntRaw(effectiveIndex(i)); }
    public int getInt(int i, int j) { return getIntRaw(effectiveIndex(i,j)); }
    public int getInt(int i, int j, int k, int... rest) {
        return getIntRaw(effectiveIndex(i, j, k, rest)); } 
    public int getInt(int[] indexes) {
        return getIntRaw(effectiveIndex(indexes));
    }

    public void set(int[] indexes, E value) {
        checkCanWrite();
        setRaw(effectiveIndex(indexes), value);
    }

  public int getLowBound(int dim)
  {
    return 0;
  }

  public int getSize (int dim)
  {
    return dim==0 ? size() : 1;
  }

    public int getElementKind() { return Sequence.OBJECT_VALUE; }

    protected RuntimeException unsupported (String text)
  {
    return unsupportedException(getClass().getName()
                                + " does not implement " + text);
  }

  public static RuntimeException unsupportedException (String text)
  {
    return new UnsupportedOperationException(text);
  }

    public E set(int index, E value) {
        checkCanWrite();
        int effi = effectiveIndex(index);
        E old = (E) getRaw(effi);
        setRaw(effi, value);
        return old;
    }

  public void fill(E value)
  {
    for (int i = startPos(); (i = nextPos(i)) != 0; )
      setPosPrevious(i, value);
  }

  public void fillPosRange(int fromPos, int toPos, E value)
  {
    int i = copyPos(fromPos);
    for (;  compare(i, toPos) < 0;  i = nextPos(i))
      setPosNext(i, value);
    releasePos(i);
  }

  public void fill(int fromIndex, int toIndex, E value)
  {
    int i = createPos(fromIndex, false);
    int limit = createPos(toIndex, true);
    for (;  compare(i, limit) < 0;  i = nextPos(i))
      setPosNext(i, value);
    releasePos(i);
    releasePos(limit);
  }

  // FIXME?
  //public final Object elementAt (int index) { return get(index); }

  /** See java.util.List. */
  public int indexOf(Object o)
  {
    int i = 0;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0;  i++)
      {
        Object v = getPosPrevious(iter);
        if (o==null ? v==null : o.equals(v))
	  {
	    releasePos(iter);
	    return i;
	  }
      }
    return -1;
  }

  /** See java.util.List. */
  public int lastIndexOf(Object o)
  {
    // FIXME use iterator?
    for (int n = size();  --n >= 0; )
      {
        Object e = get(n);
        if (o==null ? e == null : o.equals(e))
          return n;
      }
    return -1;
  }

  /** Get next matching child or descendent (ignoring attributes).
   * @param startPos starting position
   * @param type a test (predicate) to apply to selected elements
   * @param endPos stop before endPos
   * @param descend if true do depth-first traversal.
   * @return poistion of next match or 0 if none found
   */
  public int nextMatching(int startPos, ItemPredicate type,
			  int endPos, boolean descend)
  {
    if (descend)
      throw unsupported("nextMatching with descend");
    int ipos = startPos;
    for (;;)
      {
	if (compare(ipos, endPos) >= 0)
	  return 0;
	ipos = nextPos(ipos);
	if (type.isInstancePos(this, ipos))
	  return ipos;
      }
  }

  /** See java.util.List. */
  public boolean contains(Object o)
  {
    return indexOf(o) >= 0;
  }

  /** See java.util.List. */
  public boolean containsAll(Collection<?> c)
  {
    Iterator<?> i = c.iterator();
    while (i.hasNext())
      {
        Object e = i.next();
        if (! contains(e))
          return false;
      }
    return true;
  }

  public final Enumeration<E> elements()
  {
    return getIterator();
  }

  public final SeqPosition<E, AbstractSequence<E>> getIterator()
  {
    return getIterator(0);
  }

  public SeqPosition<E, AbstractSequence<E>> getIterator(int index)
  {
      return new SeqPosition<E,AbstractSequence<E>>(this, index, false);
  }

  public SeqPosition<E, AbstractSequence<E>> getIteratorAtPos(int ipos)
  {
    return new SeqPosition<E,AbstractSequence<E>>(this, copyPos(ipos));
  }

  public final Iterator<E> iterator()
  {
    return getIterator();
  }

  public final ListIterator<E> listIterator()
  {
    return getIterator(0);
  }

  public final ListIterator<E> listIterator(int index)
  {
    return getIterator(index);
  }

  /** Add a value at a specified Pos.
   * @return the updated Pos, which is after the inserted value..
   */
  protected int addPos (int ipos, E value)
  {
    throw unsupported("addPos");
  }

  /** See java.util.Collection. */
  public boolean add(E o)
  {
    addPos(endPos(), o);
    return true;
  }

  /** See java.util.List. */
  public void add(int index, E o)
  {
    int pos = createPos(index, false);
    addPos(pos, o);
    releasePos(pos);
  }

  /** See java.util.Collection. */
  public boolean addAll(Collection<? extends E> c)
  {
    return addAll(size(), c);
  }

  /** See java.util.Collection. */
  public boolean addAll(int index, Collection<? extends E> c)
  {
    boolean changed = false;
    int pos = createPos(index, false);
    for (Iterator<? extends E> it = c.iterator();  it.hasNext(); )
      {
        pos = addPos(pos, it.next());
        changed = true;
      }
    releasePos(pos);
    return changed;
  }

  /**
   * Remove one or more elements.
   * @param ipos position where elements should be removed
   * @param count if non-negative, remove that number of elements
   * following (poses, posNumber); if negative the negative of the number
   * of elements to remove before (poses, posNumber).
   * @exception java.lang.IndexOutOfBoundsException
   *   if {@code (count >= 0 ? (index < 0 || index + count > size())
   *       : (index + count < 0 || index > size()))},
   *   where {@code index == nextIndex(ipos, xpos)}.
   */
  public void removePos(int ipos, int count)
  {
    int rpos = createRelativePos(ipos, count, false);
    if (count >= 0)
      removePosRange(ipos, rpos);
    else
      removePosRange(rpos, ipos);
    releasePos(rpos);
  }

  /** Remove a range where each end-point is a position in a container.
   * @param ipos0 start of range, as a poistion
   * @param ipos1 end of range
   * @exception java.lang.IndexOutOfBoundsException
   *   if {@code nextIndex(ipos0) > nextIndex(ipos1)}
   *   or {@code nextIndex(ipos0) < 0} or {@code nextIndex(ipos1) > size()}.
   */
  protected void removePosRange(int ipos0, int ipos1)
  {
    throw unsupported("removePosRange");
  }

  public E remove(int index)
  {
    if (index < 0 || index >= size())
      throw new IndexOutOfBoundsException();
    int ipos = createPos(index, false);
    E result = (E) getPosNext(ipos);
    removePos(ipos, 1);
    releasePos(ipos);
    return result;
  }

  public boolean remove(Object o)
  {
    int index = indexOf(o);
    if (index < 0)
      return false;
    int ipos = createPos(index, false);
    removePos(ipos, 1);
    releasePos(ipos);
    return true;
  }

  public boolean removeAll(Collection<?> c)
  {
    boolean changed = false;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0; )
      {
        Object value = getPosPrevious(iter);
        if (c.contains(value))
          {
            removePos(iter, -1);
            changed = true;
          }
      }
    return changed;
  }

  public boolean retainAll(Collection<?> c)
  {
    boolean changed = false;
    for (int iter = startPos();  (iter = nextPos(iter)) != 0; )
      {
        Object value = getPosPrevious(iter);
        if (! c.contains(value))
          {
            removePos(iter, -1);
            changed = true;
          }
      }
    return changed;
  }

  public void clear()
  {
    removePos(startPos(), endPos());
  }

  /** Tests whether the position has the "isAfter" property.
   * I.e. if something is inserted at the position, will
   * the iterator end up being after the new data? */
  protected boolean isAfterPos (int ipos)
  {
    return (ipos & 1) != 0;
  }

  /** Generate a position at a given index.
   * The result is a position cookie that must be free'd with releasePos.
   * @param index offset from beginning of desired position
   * @param isAfter should the position have the isAfter property
   * @exception IndexOutOfBoundsException if index is out of bounds
   */
  public int createPos (int index, boolean isAfter) {
      return (index << 1) | (isAfter ? 1 : 0);
  }

  public int createRelativePos(int pos, int delta, boolean isAfter)
  {
    return createPos(nextIndex(pos) + delta, isAfter);
  }

  public int startPos () { return 0; }
  public int endPos () { return -1; }

  /**
   * Reclaim any resources used by the given position int.
   * @param ipos the Pos being free'd.
   */
  protected void releasePos(int ipos)
  {
  }

  /** Make a copy of a position int.
   * For simple positions returns the argument.
   * However, if the positions are magic cookies that are actively managed
   * by the sequence (as opposed to for example a simple index), then making
   * a copy may need to increment a reference count, or maybe allocate a
   * new position cookie.  In any case, the new position is initialized to
   * the same offset (and isAfter property) as the original.
   * @param ipos the position being copied.
   * @return the new position
   */
  public int copyPos (int ipos)
  {
    return ipos;
  }

  /** Get offset of (ipos1) relative to (ipos0). */
  protected int getIndexDifference(int ipos1, int ipos0)
  {
    return nextIndex(ipos1) - nextIndex(ipos0);
  }

  /**
   * Get the offset from the beginning corresponding to a position cookie.
   */
  protected int nextIndex(int ipos) {
    return ipos == -1 ? size() : ipos >>> 1;
  }
    public int xnextIndex(int ipos) {
        return nextIndex(ipos);
    }

  protected int fromEndIndex(int ipos)
  {
    return size() - nextIndex(ipos);
  }

  /**
   * Get the size of the (sub-) sequence containing a given position.
   * Normally the same as size(), but may be different if this Sequence
   * is a tree and the position points at an interior node.
   */
  protected int getContainingSequenceSize(int ipos)
  {
    return size();
  }

  public boolean hasNext (int ipos)
  {
    return nextIndex(ipos) != size();
  }

  public int getNextKind(int ipos)
  {
    return hasNext(ipos) ? Sequence.OBJECT_VALUE : Sequence.EOF_VALUE;
  }

  public String getNextTypeName(int ipos)
  {
    Object type = getNextTypeObject(ipos);
    return type == null ? null : type.toString();
  }

  public E getNextTypeObject(int ipos)
  {
    return null;
  }

  /** Called by SeqPosition.hasPrevious. */
  protected boolean hasPrevious(int ipos)
  {
    return nextIndex(ipos) != 0;
  }

  /** Return the next position following the argument.
   * The new position has the isAfter property.
   * The argument is implicitly released (as in releasePos).
   * Returns 0 if we are already at end of file.
   */
  public int nextPos (int ipos)
  {
    if (! hasNext(ipos))
      return 0;
    int next = createRelativePos(ipos, 1, true);
    releasePos(ipos);
    return next;
  }

  /** Return the previous position following the argument.
   * The new position has the isBefore property.
   * The argument is implicitly released (as in releasePos).
   * Returns -1 if we are already at beginning of file.
   */
  public int previousPos (int ipos)
  {
    if (! hasPrevious(ipos))
      return 0;
    int next = createRelativePos(ipos, -1, false);
    releasePos(ipos);
    return next;
  }

  /** Set position before first child (of the element following position).
   * @return true if there is a child sequence (which might be empty);
   *   false if current position is end of sequence or following element
   *   is atomic (cannot have children).
   */
  public final boolean gotoChildrenStart(TreePosition pos)
  {
    int ipos = firstChildPos(pos.getPos());
    if (ipos == 0)
      return false;
    pos.push(this, ipos);
    return true;
  }

  /** Get position before first child (of the element following position).
   * @param ipos parent position.  It is not released by this method.
   * @return non-zero position cookie if there is a child sequence
   *   (which might be empty);  zero if current position is end of sequence
   *   or following element is atomic (cannot have children).
   */
  public int firstChildPos (int ipos)
  {
    return 0;
  }

  public int firstChildPos (int ipos, ItemPredicate predicate)
  {
    int child = firstChildPos(ipos);
    if (child == 0)
      return 0;
    if (predicate.isInstancePos(this, child))
      return child;
    else
      return nextMatching(child, predicate, endPos(), false);
  }

  /** Like firstChildPos.
   * Problem: Should this stop before we get to children?
   * I think so, but that requires changes to TreeList. */
  public int firstAttributePos (int ipos)
  {
    return 0;
  }

  /** Get position of parent.
   * @param ipos child position.  It is not released by this method.
   * @return the p os of the parent, or endPos() is there is no known parent.
   */
  public int parentPos (int ipos)
  {
    return endPos();
  }

  protected boolean gotoParent(TreePosition pos)
  {
    if (pos.depth < 0)
      return false;
    pos.pop();
    return true;
  }

  public int getAttributeLength()
  {
    return 0;
  }

  public Object getAttribute(int index)
  {
    return null;
  }

  protected boolean gotoAttributesStart(TreePosition pos)
  {
    return false;
  }

  /** Get the element following the specified position.
   * @param ipos the specified position.
   * @return the following element, or eofValue if there is none.
   * Called by SeqPosition.getNext.
   * FIXME Should change eof handling so return type can be E.
   */
  public Object getPosNext(int ipos)
  {
    if (! hasNext(ipos))
      return Sequence.eofValue;
    return get(nextIndex(ipos));
  }

  /** Get the element before the specified position.
   * @param ipos the specified position.
   * @return the following element, or eofValue if there is none.
   * FIXME Should change eof handling so return type can be E.
   */
  public Object getPosPrevious(int ipos)
  {
    int index = nextIndex(ipos);
    if (index <= 0)
      return Sequence.eofValue;
    return get(index - 1);
  }

  protected void setPosNext(int ipos, E value)
  {
    int index = nextIndex(ipos);
    if (index >= size())
      throw new IndexOutOfBoundsException();
    set(index, value);
  }

  protected void setPosPrevious(int ipos, E value)
  {
    int index = nextIndex(ipos);
    if (index == 0)
      throw new IndexOutOfBoundsException();
    set(index - 1, value);
  }

  public final int nextIndex(SeqPosition pos)
  {
    return nextIndex(pos.ipos);
  }

  /** Compare two positions, and indicate if they are the same position. */
  public boolean equals(int ipos1, int ipos2)
  {
    return compare(ipos1, ipos2) == 0;
  }

  /** Compare two positions, and indicate their relative order. */
  public int compare(int ipos1, int ipos2)
  {
    int i1 = nextIndex(ipos1);
    int i2 = nextIndex(ipos2);
    return i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
  }

  public final int compare(SeqPosition i1, SeqPosition i2)
  {
    return compare(i1.ipos, i2.ipos);
  }

  public Object[] toArray() 
  { 
    int len = size(); 
    Object[] arr = new Object[len];
    int it = startPos();
    int i = 0;
    while ((it = nextPos(it)) != 0)
      arr[i++] = getPosPrevious(it);
    return arr;
  } 

  public <T> T[] toArray(T[] arr) 
  { 
    int alen = arr.length; 
    int len = size(); 
    if (len > alen) 
    { 
      Class componentType = arr.getClass().getComponentType();
      arr = (T[]) java.lang.reflect.Array.newInstance(componentType, len);
      alen = len; 
    }
    
    int it = startPos();
    for (int i = 0;  (it = nextPos(it)) != 0; i++)
    {
      arr[i] = (T) getPosPrevious(it);
    } 
    if (len < alen) 
      arr[len] = null; 
    return arr;
  }

  /** This is used for the XML concept of "document order". */
  public int stableCompare (AbstractSequence other)
  {
    int id1 = System.identityHashCode(this);
    int id2 = System.identityHashCode(other);
    return id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
  }

  /** This is used for the XML concept of "document order".
   * It is overridden in gnu.xml.NodeTree for a more robust implementation.
   */
  public static int compare(AbstractSequence seq1, int pos1,
			    AbstractSequence seq2, int pos2)
  {
    if (seq1 == seq2)
      return seq1.compare(pos1, pos2);
    return seq1.stableCompare(seq2);
  }

  public int hashCode()
  {
    if (rank() != 1 && this instanceof Array)
        return Arrays.hashCode((Array) this);
    // Implementation specified by the Collections specification.
    int hash = 1;
    for (int i = startPos(); (i = nextPos(i)) != 0;  )
      {
	Object obj = getPosPrevious(i);
	hash = 31*hash + (obj==null ? 0 : obj.hashCode());
      }
    return hash;
  }

    public int boundedHash(int seed, int limit) {
        int count = 0;
        int sublimit = limit >> 1;
        for (int i = startPos(); (i = nextPos(i)) != 0;  )  {
            if (++count > limit)
                break;
            int h = HashUtils.boundedHash(getPosPrevious(i), 0, sublimit);
            seed = HashUtils.murmur3step(seed, h);
        }
        return HashUtils.murmur3finish(seed, count);
   }

  public boolean equals(Object o)
  {
    // Compatible with the Collections specification.
    // FIXME should also depend on class?
    if (! (this instanceof java.util.List)
        || ! (o instanceof java.util.List))
      return this == o;
    Iterator<E> it1 = iterator();
    Iterator<E> it2 = ((java.util.List<E>) o).iterator();
    for (;;)
      {
        boolean more1 = it1.hasNext();
        boolean more2 = it2.hasNext();
        if (more1 != more2)
          return false;
        if (! more1)
          return true;
        E e1 = it1.next();
        E e2 = it2.next();
	if (e1 == null)
          {
            if (e2 != null)
              return false;
          }
        else if (! e1.equals(e2))
          return false;
      }
  }

  public Sequence subSequence(SeqPosition start, SeqPosition end)
  {
    return subSequencePos(start.ipos, end.ipos);
  }

  protected Sequence<E> subSequencePos(int ipos0, int ipos1)
  {
    return new SubSequence<E>(this, ipos0, ipos1);
  }

  public List<E> subList(int fromIx, int toIx)
  {
    return subSequencePos(createPos(fromIx, false),
                          createPos(toIx, true));
  }

  /** Copy an element specified by a position pair to a Consumer.
   * @return if hasNext(ipos). */
  public boolean consumeNext (int ipos, Consumer out)
  {
    int next = nextPos(ipos);
    if (next == 0)
      return false;
    consumePosRange(ipos, next, out);
    return true;
  }

  public void consumePosRange(int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int it = copyPos(iposStart);
    while (! equals(it, iposEnd))
      {
	if (! hasNext(it))
          throw new RuntimeException();
	out.writeObject(getPosNext(it));
        it = nextPos(it);
      }
    releasePos(it);
  }
  
    public void consume(int fromIndex, int toIndex, Consumer out) {
        int ipos0 = createPos(fromIndex, false);
        int ipos1 = createPos(toIndex, true);
        consumePosRange(ipos0, ipos1, out);
        releasePos(ipos0);
        releasePos(ipos1);
    }

  public void consume(Consumer out)
  {
    boolean isSequence = this instanceof Sequence;
    if (isSequence)
      out.startElement("#sequence");
    consumePosRange(startPos(), endPos(), out);
    if (isSequence)
      out.endElement();
  }

  public void toString (String sep, StringBuffer sbuf)
  {
    boolean seen = false;
    for (int i = startPos();  (i = nextPos(i)) != 0; )
      {
	if (seen)
	  sbuf.append(sep);
	else
	  seen = true;
	sbuf.append(getPosPrevious(i));
      }
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (this instanceof Sequence)
      sbuf.append('[');
    toString(", ", sbuf);
    if (this instanceof Sequence)
      sbuf.append(']');
    return sbuf.toString();
  }
}

// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * A position that can also go down and up in a tree. 
 * A TreePosition is a stack of positions.  The "current" position
 * (i.e. the one you get if you tree the TreePosition as a SeqPosition)
 * is that in the innermost containing sequence.
 *
 * Normally, the "current" element is (the one following) a position in a
 * sequence.  As a special (initial case), we may want to treat the
 * entire sequence is the "current element".  This is represented by depth==-1
 * and xpos set to the root element (which need not actually be a sequence).
 */

public class TreePosition extends SeqPosition implements Cloneable
{
  /** Stack of pushed values for sequence. */
  AbstractSequence[] sstack;

  /** Stack of pushed values for iposition. */
  int[] istack; 

  /** Stack of pushed values for xposition. */
  Object[] xstack;

  /** Depth of the above stacks.
   * Note that getDepth returns depth+1; this should perhaps match. */
  int depth;

  public TreePosition()
  {
    depth = -1;
  }

  /** Not a position *in* a sequence, but the current element is the entire sequence. */
  public TreePosition(Object root)
  {
    xpos = root;
    depth = -1;
  }

  public TreePosition(AbstractSequence seq, int index)
  {
    seq.makePosition(index, false, this, 0);
  }

  public TreePosition (TreePosition pos)
  {
    set(pos);
  }

  public Object clone ()
  {
    return new TreePosition(this);
  }

  public void set (TreePosition position)
  {
    clear();
    int d = position.depth;
    depth = d;
    if (d < 0)
      {
	xpos = position.xpos;
	return;
      }
    if (sstack == null || sstack.length <= d)
      sstack = new AbstractSequence[d + 10];
    if (istack == null || istack.length <= d)
      istack = new int[d + 10];
    if (xstack == null || xstack.length <= d)
      xstack = new Object[d + 10];
    AbstractSequence seq;
    int i;
    for (i = 0;  i < depth;  i++)
      {
	seq = position.sstack[i];
	seq.copyPosition(position.istack[i], position.xstack[i],
			 this, depth - i);
      }
    seq = position.sequence;
    seq.copyPosition(position.ipos, position.xpos, this, 0);
  }

  /** Number of ancestor sequences, including current sequence. */
  public int getDepth()
  {
    return depth + 1;
  }

  /** Get the "root document". */
  public AbstractSequence getRoot()
  {
    return depth == 0 ? sequence : sstack[0];
  }

  public Object getNext()
  {
    return sequence == null ? xpos : sequence.getNext(ipos, xpos);
  }

  public void push(AbstractSequence child, int iposChild, Object xposChild)
  {
    if (depth >= 0)
      {
	if (depth == 0)
	  {
	    istack = new int[8];
	    xstack = new Object[8];
	    sstack = new AbstractSequence[8];
	  }
	else if (depth >= istack.length)
	  {
	    int ndepth = 2 * depth;
	    int[] itemp = new int[ndepth];
	    Object[] xtemp = new Object[ndepth];
	    AbstractSequence[] stemp = new AbstractSequence[ndepth];
	    System.arraycopy(istack, 0, itemp, 0, depth);
	    System.arraycopy(xstack, 0, xtemp, 0, depth);
	    System.arraycopy(sstack, 0, stemp, 0, depth);
	    istack = itemp;
	    xstack = xtemp;
	    sstack = stemp;
	  }
	sstack[depth] = sequence;
	istack[depth] = ipos;
	xstack[depth] = xpos;
      }
    depth++;
    sequence = child;
    ipos = iposChild;
    xpos = xposChild;
  }

  public void pop()
  {
    sequence.releasePosition(ipos, xpos);
    popNoRelease();
  }

  public void popNoRelease()
  {
    if (--depth < 0)
      {
	xpos = sequence;
	sequence = null;
      }
    else
      {
	sequence = sstack[depth];
	ipos = istack[depth];
	xpos = xstack[depth];
      }
  }

  public final boolean gotoParent()
  {
    return sequence == null ? false : sequence.gotoParent(this);
  }

  /** Set position before first child (of the element following position).
   * @return true if there is a child sequence (which might be empty);
   *   false if current position is end of sequence or following element
   *   is atomic (cannot have children).
   */
  public boolean gotoChildrenStart()
  {
    if (sequence == null)
      {
	if (! (xpos instanceof AbstractSequence))
	  return false;
	depth = 0;
	sequence = (AbstractSequence) xpos;
	sequence.makeStartPosition(this, 0);
      }
    else
      {
	if (! sequence.gotoChildrenStart(this))
	  return false;
      }
    return true;
  }

  /** Set position before first attribute (of the element following position).
   * This is used to iterate through the sequence of attributes.
   */
  public boolean gotoAttributesStart()
  {
    if (sequence == null)
      {
	if (xpos instanceof AbstractSequence)
	  {
	    // ??? FIXME
	  }
	return false;
      }
    return sequence.gotoAttributesStart(this);
  }

  /*
  public boolean gotoAttribute(Object name)
  {
    return sequence.gotoAttribute(this);
  }
  */

  /** Get the value of an ancestor node.
   * @param up the number parents to go up.
   * @return if up is 0, same getNext.   Otherwise get parent
   * applied as specified.
   */
  public Object getAncestor(int up)
  {
    if (up == 0)
      return sequence.getNext(ipos, xpos);
    int i = depth - up;
    if (i <= 0)
      return getRoot();
    return sstack[i].getNext(istack[i], xstack[i]);
  }

  public void finalize()
  {
    clear();
  }

  public void clear()
  {
    while (sequence != null)
      {
        sequence.releasePosition(ipos, xpos);
        pop();
      }
    xpos = null;
  }

  /** Copy this position into pos. */
  /*
  public void clone (Position pos)
  {
    // FIXME!
  }

  public Object clone()
  {
    TreePosition pos = new TreePosition();
    clone(pos);
    return pos;
  }
  */

  /** Implements PositionContainer. */
  public int getPositionInt(int positionNumber)
  { return positionNumber == 0 ? ipos : istack[depth-positionNumber]; }

  /** Implements PositionContainer. */
  public Object getPositionPtr(int positionNumber)
  { return positionNumber == 0 ? xpos : xstack[depth-positionNumber]; }

  /** Implements PositionContainer. */
  public void setPosition(int positionNumber, int ipos, Object xpos)
  {
    if (positionNumber == 0)
      {	this.ipos = ipos;  this.xpos = xpos; }
    else
      {
	istack[depth-positionNumber] = ipos;
	xstack[depth-positionNumber] = xpos;
      }
  }

  /** Implements PositionContainer. */
  public void setSequence(int positionNumber, AbstractSequence seq)
  {
    if (positionNumber == 0)
      this.sequence = seq;
    else
      sstack[depth-positionNumber] = seq;
  }

  /** Implements PositionContainer. */
  public int countPositions() { return depth + 1; }

  public void dump()
  {
    System.err.println("TreePosition dump depth:"+depth);
    if (depth < 0)
      {
	System.err.println("#- xpos:"+getPositionPtr(0));
      }
    for (int i = 0;  i <= depth;  i++)
      {
	AbstractSequence seq = i==0 ? sequence : sstack[depth-i];
	System.err.print("#"+i+" seq:"+seq);
	System.err.println(" ipos:"+getPositionInt(i)+" xpos:"+getPositionPtr(i));
      }
  }
}
// This is for people using the Emacs editor:
// Local Variables:
// c-file-style: "gnu"
// tab-width: 8
// indent-tabs-mode: t
// End:

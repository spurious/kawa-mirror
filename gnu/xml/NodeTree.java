// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;
import gnu.mapping.CharArrayOutPort;

/** Use to represent a Document or Document Fragment, in the XML DOM sense.
 * More compact than traditional DOM, since it uses many fewer objects.
 */

public class NodeTree extends TreeList
{
  public boolean writePosition(AbstractSequence seq, int ipos)
  {
    seq.consumeNext(ipos, this);
    return true;
  }

  /** If v is a node, make a copy of it. */
  public void writeObject(Object v)
  {
    if (v instanceof SeqPosition)
      {
	SeqPosition pos = (SeqPosition) v;
	writePosition(pos.sequence, pos.getPos());
      }
    else if (v instanceof TreeList)
      ((TreeList) v).consume(this);
    else
      super.writeObject(v);
  }

  public int nextPos (int position)
  {
    boolean isAfter = (position & 1) != 0;
    int index = posToDataIndex(position);
    int next = nextNodeIndex(index, -1 >>> 1);
    if (next != index)
      return next << 1;
    if (index == data.length)
      return 0;
    return (index << 1) + 3;
  }

  public static NodeTree make ()
  {
    return new NodeTree();
  }

  static int counter;
  int id;

  /** Get/create a new unique number. */
  public int getId()
  {
    if (id == 0)
      id = ++counter;
    return id;
  }

  public int stableCompare (AbstractSequence other)
  {
    if (this == other)
      return 0;
    // If other is also a NodeTree it would be simpler to just compare
    // the results of getId, but if we always did that there is the
    // slight risk that counter could overflow in the case of a
    // long-running program.  So we use system.identityHashCode as
    // the primary "key" and getId only when needed as a tie-breaker.
    int comp = super.stableCompare(other);
    if (comp == 0 && other instanceof NodeTree)
      {
	int id1 = this.getId();
	int id2 = ((NodeTree) other).getId();
	comp = id1 < id2 ? -1 : id1 > id2 ? 1 : 0;
      }
    return comp;
  }


  public String toString ()
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    XMLPrinter xp = new XMLPrinter(wr);
    consume(xp);
    return wr.toString();
  }
}

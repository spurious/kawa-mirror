// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.lists.*;

/** Use to represent a Document of Document Fragment, in the XML DOM sense.
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

  public static NodeTree make ()
  {
    return new NodeTree();
  }
}

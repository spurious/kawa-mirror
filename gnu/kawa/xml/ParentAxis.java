// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a parent:: step in a path expression. */

public class ParentAxis extends TreeScanner
{
  public static ParentAxis make (NodePredicate type)
  {
    ParentAxis axis = new ParentAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    int parent = seq.parentPos(ipos);
    int end = seq.endPos();
    if (parent != end && type.isInstancePos(seq, parent))
      out.writePosition(seq, ipos);
  }
}

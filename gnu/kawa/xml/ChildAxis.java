// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a child:: step in a path expression. */

public class ChildAxis extends TreeScanner
{
  public static ChildAxis make (NodePredicate type)
  {
    ChildAxis axis = new ChildAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    int child = seq.firstChildPos(ipos);
    TreeList tl = (TreeList)seq;
    if (child == 0)
      return;
    int limit = seq.nextPos(ipos);
    if (type.isInstancePos(seq, child))
      out.writePosition(seq, child);
    for (;;)
      {
	child = seq.nextMatching(child, type, limit, false);
	if (child == 0)
	  break;
	out.writePosition(seq, child);
      }
  }
}

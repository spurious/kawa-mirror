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
    ipos = seq.firstChildPos(ipos);
    while (ipos != 0)
      {
	if (type.isInstancePos(seq, ipos))
	  out.writePosition(seq, ipos);
	ipos = seq.nextPos(ipos);
      }
  }
}

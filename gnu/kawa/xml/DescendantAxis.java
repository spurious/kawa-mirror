// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a descendant:: step in a path expression. */

public class DescendantAxis extends TreeScanner
{
  public static DescendantAxis make (NodePredicate type)
  {
    DescendantAxis axis = new DescendantAxis();
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
	scan(seq, ipos, out);
	ipos = seq.nextPos(ipos);
      }
  }
}

// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement a descendant-or-self:: step in a path expression. */

public class DescendantOrSelfAxis extends TreeScanner
{
  public static DescendantOrSelfAxis make (NodePredicate type)
  {
    DescendantOrSelfAxis axis = new DescendantOrSelfAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    if (type.isInstancePos(seq, ipos))
      out.writePosition(seq, ipos);
    ipos = seq.firstChildPos(ipos);
    while (ipos != 0)
      {
	scan(seq, ipos, out);
	ipos = seq.nextPos(ipos);
      }
  }
}

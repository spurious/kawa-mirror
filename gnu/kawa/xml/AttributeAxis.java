// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;

/** Used to implement an attribute:: step in a path expression. */

public class AttributeAxis extends TreeScanner
{
  public static AttributeAxis make (NodePredicate type)
  {
    AttributeAxis axis = new AttributeAxis();
    axis.type = type;
    return axis;
  }

  public void scan (AbstractSequence seq, int ipos, PositionConsumer out)
  {
    ipos = seq.firstAttributePos(ipos);
    // FIXME this also interates over children, at least for TreeList.
    while (ipos != 0)
      {
	if (type.isInstancePos(seq, ipos))
	  out.writePosition(seq, ipos);
	ipos = seq.nextPos(ipos);
      }
  }
}

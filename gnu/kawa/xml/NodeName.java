// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class NodeName extends Procedure1
{
  public static NodeName nodeName = new NodeName();

  public static Object nodeName (Object node)
  {
    Object nodeName = null;
    if (node instanceof AbstractSequence)
      {
	AbstractSequence seq = (AbstractSequence) node;
	nodeName = seq.getNextTypeObject(seq.startPos());
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	nodeName = pos.sequence.getNextTypeObject(pos.ipos);
      }
    if (nodeName == null)
      return Values.empty;
    if (nodeName instanceof String)
      return Symbol.make("", nodeName.toString(), "");
    return nodeName;
  }

  public Object apply1 (Object node)
  {
    return nodeName(node);
  }
}


// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;

public class Children extends CpsProcedure
{
  public static final Children children = new Children();
  
  public int numArgs() { return 0x1001; }

  public static void children (TreeList tlist, int index, Consumer consumer)
  {
    int child = tlist.gotoChildrenStart(index);
    while (child >= 0)
      {
	int ipos = child << 1;
	int kind = tlist.getNextKind(ipos, null);
	if (kind == Sequence.EOF_VALUE)
	  break;
	// if kind is CHAR_VALUE return text node.  FIXME
	int next = tlist.nextDataIndex(child);
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(tlist,  ipos, null);
	else
	  tlist.consumeRange(child, next, consumer);
	child = next;
      }
  }

  public static void children (Object node, Consumer consumer)
  {
    if (node instanceof TreeList)
      {
	children((TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  children((TreeList) pos.sequence, pos.ipos >> 1, consumer);
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    ctx.lastArg();
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1, null);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      children(tlist.getNext(index << 1, null), consumer);
	    else
	      children(tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      children(node, consumer);
  }
}

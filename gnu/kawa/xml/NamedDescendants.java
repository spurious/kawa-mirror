// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class NamedDescendants extends CpsProcedure
{
  public static final NamedDescendants namedDescendants
   = new NamedDescendants(false);

  public static final NamedDescendants namedDescendantsOrSelf
   = new NamedDescendants(true);

  boolean orSelf;

  public NamedDescendants (boolean orSelf)
  {
    this.orSelf = orSelf;
  }

  public static void namedDescendants(NodePredicate predicate,
				      TreeList list, int pos,
				      Consumer consumer, boolean orSelf)
  {
    int limit = list.nextDataIndex(pos);
    if (orSelf && predicate.isInstancePos(list, pos << 1))
      {
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(list, pos << 1);
	else
	  {
	    int next = list.nextNodeIndex(pos, -1 >>> 1);
	    if (pos == next)
	      next = list.nextDataIndex(pos);
	    list.consumeIRange(pos, next, consumer);
	  }
      }
    for (;;)
      {
	pos = list.nextMatchingChild(pos, predicate, limit);
	if (pos < 0)
	  break;
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(list, pos << 1);
	else
	  {
	    int next = list.nextNodeIndex(pos, -1 >>> 1);
	    if (pos == next)
	      next = list.nextDataIndex(pos);
	    list.consumeIRange(pos, next, consumer);
	  }
      }
  }

  public static void namedDescendants (NodePredicate predicate, Object node,
				       Consumer consumer, boolean orSelf)
    throws Throwable
  {
    if (node instanceof TreeList)
      {
	namedDescendants(predicate, (TreeList) node, 0, consumer, orSelf);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) pos.sequence;
	    namedDescendants(predicate, tlist,
			     tlist.posToDataIndex(pos.ipos),
			     consumer, orSelf);
	  }
      }
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    NodePredicate predicate = (NodePredicate) ctx.getNextArg();
    ctx.lastArg();
    if (node instanceof Values)
      {
	TreeList tlist = (TreeList) node;
	int index = 0;
	for (;;)
	  {
	    int kind = tlist.getNextKind(index << 1);
	    if (kind == Sequence.EOF_VALUE)
	      break;
	    if (kind == Sequence.OBJECT_VALUE)
	      namedDescendants(predicate, tlist.getPosNext(index << 1),
			       consumer, orSelf);
	    else
	      namedDescendants(predicate, tlist, index, consumer, orSelf);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      namedDescendants(predicate, node, consumer, orSelf);
  }
}


// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;

public class NamedDescendants extends CpsProcedure
{
 public static final NamedDescendants namedDescendants
   = new NamedDescendants();

  public static void namedDescendants(String namespaceURI, String localName,
				      TreeList list, int pos,
				      Consumer consumer)
  {
    GroupPredicate predicate = new ElementType(namespaceURI, localName);
    int limit = list.nextDataIndex(pos);
    for (;;)
      {
	pos = list.nextMatchingChild(pos, predicate, limit);
	if (pos < 0)
	  break;
	if (consumer instanceof PositionConsumer)
	  ((PositionConsumer) consumer).writePosition(list, pos << 1, null);
	else
	  {
	    int next = list.nextDataIndex(pos);
	    list.consumeRange(pos, next, consumer);
	  }
      }
  }

  public static void namedDescendants (String namespaceURI, String localName, Object node, Consumer consumer)
    throws Throwable
  {
    if (node instanceof TreeList)
      {
	namedDescendants(namespaceURI, localName, (TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  namedDescendants(namespaceURI, localName, (TreeList) pos.sequence, pos.ipos >> 1, consumer);
      }
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    String namespaceURI = (String) ctx.getNextArg();
    String localName = (String) ctx.getNextArg();
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
	      namedDescendants(namespaceURI, localName, tlist.getNext(index << 1, null), consumer);
	    else
	      namedDescendants(namespaceURI, localName, tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      namedDescendants(namespaceURI, localName, node, consumer);
  }
}


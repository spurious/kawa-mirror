// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

/** Helper procedure to implement PATH/@ATTR and PATH/*. */

public class NamedAttributes extends CpsProcedure
{
  public static final NamedAttributes namedAttributes = new NamedAttributes();
  
  public int numArgs() { return 0x2002; }

  public static void namedAttributes (Symbol qname,
				      TreeList tlist, int index,
				      Consumer consumer)
  {
    String namespaceURI = qname.getNamespaceURI();
    String localName = qname.getLocalName();
    int child = tlist.gotoAttributesStart(index);
    while (child >= 0)
      {
	int ipos = child << 1;
	int kind = tlist.getNextKind(ipos);
	if (kind != Sequence.ATTRIBUTE_VALUE)
	  break;
	int next = tlist.nextDataIndex(child);

	Object curName = tlist.getNextTypeObject(ipos);
	String curNamespaceURI;
	String curLocalName;
	if (curName instanceof Symbol)
	  {
	    Symbol cname = (Symbol) curName;
	    curNamespaceURI = cname.getNamespaceURI();
	    curLocalName = cname.getLocalName();
	  }
	else
	  {
	    curNamespaceURI = "";
	    curLocalName = curName.toString().intern();  // FIXME
	  }
	if ((localName == curLocalName || localName == null)
	    && (namespaceURI == curNamespaceURI || namespaceURI == null))
	  {
	    if (consumer instanceof PositionConsumer)
	      ((PositionConsumer) consumer).writePosition(tlist,  ipos);
	    else
	      tlist.consumeIRange(child, next, consumer);
	  }
	child = next;
      }
  }

  public static void namedAttributes (Symbol qname, Object node, Consumer consumer)
  {
    if (node instanceof TreeList)
      {
	namedAttributes(qname, (TreeList) node, 0, consumer);
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	if (pos.sequence instanceof TreeList)
	  {
	    TreeList tlist = (TreeList) pos.sequence;
	    namedAttributes(qname, tlist,
			    tlist.posToDataIndex(pos.ipos),
			    consumer);
	  }
      }
  }

  public void apply (CallContext ctx)
  {
    Consumer consumer = ctx.consumer;
    Object node = ctx.getNextArg();
    Symbol qname = (Symbol) ctx.getNextArg();
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
	      namedAttributes(qname, tlist.getPosNext(index << 1), consumer);
	    else
	      namedAttributes(qname, tlist, index, consumer);
	    index = tlist.nextDataIndex(index);
	  }
      }
    else
      namedAttributes(qname, node, consumer);
  }
}

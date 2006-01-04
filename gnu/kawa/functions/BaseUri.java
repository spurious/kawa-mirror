// Copyright (c) 2003, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.lists.*;

public class BaseUri
{
  public static Object baseUri (Object node)
  {
    Object baseUri = null;
    if (node instanceof AbstractSequence)
      {
	AbstractSequence seq = (AbstractSequence) node;
	baseUri = seq.baseUriOfPos(seq.startPos());
      }
    else if (node instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) node;
	baseUri = pos.sequence.baseUriOfPos(pos.ipos);
      }
    return baseUri == null ? Values.empty : baseUri;
  }

  public static Object baseUri ()
  {
    CallContext ctx = CallContext.getInstance();
    String baseUri = ctx.getBaseUri();
    return baseUri == null ? Values.empty : (Object) baseUri;
  }
}

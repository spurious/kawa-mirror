// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.*;

public class BaseUri extends Procedure1
{
  public static final BaseUri baseUri = new BaseUri();

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

  public Object apply1 (Object node)
  {
    return baseUri(node);
  }
}

// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.lists.*;

public class ValueStack extends Values implements Sequence
{
  public void clear()
  {
    oindex = 0;
    super.clear();
  }
}

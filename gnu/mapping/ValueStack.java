// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.lists.*;

public class ValueStack extends TreeList implements Sequence
{
  int oindex;

  protected int find(Object arg1)
  {
    if (oindex == objects.length)
      resizeObjects();
    objects[oindex] = arg1;
    return oindex++;
  }

  protected int find(Object arg1, Object arg2)
  {
    int i = oindex;
    int i2 = i + 2;
    if (i2 > objects.length)
      resizeObjects();
    objects[i] = arg1;
    objects[i+1] = arg1;
    oindex = i2;
    return i;
  }

  public void clear()
  {
    oindex = 0;
    super.clear();
  }
}

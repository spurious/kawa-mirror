// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.Values;
import gnu.mapping.*;
import gnu.lists.*;

public class SequenceUtils
{
  public static boolean isEmptySequence(Object arg)
  {
    return arg instanceof Values && ((Values) arg).isEmpty();
  }

  public static boolean exists (Object arg)
  {
    return ! (arg instanceof Values && ((Values) arg).isEmpty());
  }

  /** Implements the standard XQuery function {@code reverse}. */
  public static void reverse$X (Object arg, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (! (arg instanceof Values))
      {
        out.writeObject(arg);
        return;
      }
    Values vals = (Values) arg;
    int ipos = 0;
    int[] poses = new int[100];
    int n = 0;
    for (;;)
      {
        if (n >= poses.length)
          {
            int[] t = new int[2 * n];
            System.arraycopy(poses, 0, t, 0, n);
            poses = t;
          }
        poses[n++] = ipos;
        ipos = vals.nextPos(ipos);
        if (ipos == 0)
          break;
      }
    int count;
    for (int i = n-1;  --i >= 0; )
      vals.consumePosRange(poses[i], poses[i+1], out);
  }
}

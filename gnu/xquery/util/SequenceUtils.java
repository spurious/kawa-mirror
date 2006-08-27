// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.Values;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.kawa.xml.KNode;

public class SequenceUtils
{
  public static Object zeroOrOne (Object arg)
  {
    // Assumes arg is normalized.
    if (arg instanceof Values && ! ((Values) arg).isEmpty())
      throw new IllegalArgumentException();
    return arg;
  }

  public static Object oneOrMore (Object arg)
  {
    if (arg instanceof Values && ((Values) arg).isEmpty())
      throw new IllegalArgumentException();
    return arg;
  }

  public static Object exactlyOne (Object arg)
  {
    // Assumes arg is normalized.
    if (arg instanceof Values)
      throw new IllegalArgumentException();
    return arg;
  }

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

  public static void indexOf$X (Object seqParam, Object srchParam,
                                NamedCollator collator, CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (seqParam instanceof Values)
      {
        Values vals = (Values) seqParam;
        int ipos = vals.startPos();
        int i = 1;
        for (; (ipos = vals.nextPos(ipos)) != 0; i++)
          if (Compare.apply(Compare.TRUE_IF_EQU,
                            vals.getPosPrevious(ipos),
                            srchParam, collator))
            out.writeInt(i);
      }
    else if (Compare.apply(Compare.TRUE_IF_EQU, seqParam, srchParam, collator))
      out.writeInt(1);
  }

  public static boolean deepEqual (Object arg1, Object arg2,
                                   NamedCollator collator)
  {
    if (arg1 == arg2)
      return true;
    if (arg1 == null || arg1 == Values.empty)
      return arg2 == null || arg2 == Values.empty;
    if (arg2 == null || arg2 == Values.empty)
      return false;
    int ipos1 = 1, ipos2 = 1;
    boolean is1seq = arg1 instanceof Values;
    boolean is2seq = arg2 instanceof Values;
    Values vals1 = is1seq ? (Values) arg1 : null;
    Values vals2 = is2seq ? (Values) arg2 : null;
    boolean first = true;
    for (;;)
      {
        if (is1seq)
          {
            if (first)
              ipos1 = vals1.startPos();
            ipos1 = vals1.nextPos(ipos1);
          }
        if (is2seq)
          {
            if (first)
              ipos2 = vals2.startPos();
            ipos2 = vals2.nextPos(ipos2);
          }
        if (ipos1 == 0 || ipos2 == 0)
          return ipos1 == ipos2;
        Object item1 = is1seq ? vals1.getPosPrevious(ipos1) : arg1;
        Object item2 = is2seq ? vals2.getPosPrevious(ipos2) : arg2;

        if (! (item1 instanceof KNode) && !( item2 instanceof KNode))
          {
            try
              {
                if (! Compare.atomicCompare(Compare.TRUE_IF_EQU, arg1, arg2, collator))
                  return false;
              }
            catch (Throwable ex)
              {
                return false;
              }
          }
        else if (item1 instanceof KNode && item2 instanceof KNode)
          {
            KNode node1 = (KNode) item1;
            KNode node2 = (KNode) item2;
            int kind1 = node1.getNodeType();
            int kind2 = node2.getNodeType();
            if (kind1 != kind2)
              return false;
            switch (kind1)
              {
              default:
                if (! item1.equals(item2)) // FIXME - use eq
                  return false;
              }
          }
        else
          return false;

        if (first)
          {
            first = false;
            if (! is1seq)
              ipos1 = 0;
            if (! is2seq)
              ipos2 = 0;
          }
      }
  }
}

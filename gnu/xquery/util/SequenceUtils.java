// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.Values;

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
}

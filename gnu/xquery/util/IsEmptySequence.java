// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;

public class IsEmptySequence extends Procedure1
{
  public static final IsEmptySequence isEmptySequence = new IsEmptySequence();

  public static boolean isEmptySequence(Object arg)
  {
    return arg instanceof Values && ((Values) arg).isEmpty();
  }

  public Object apply1(Object arg)
  {
    return isEmptySequence(arg) ? Boolean.TRUE : Boolean.FALSE;
  }
}


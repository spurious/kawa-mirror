// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A predicate (or type) on an element of a sequence.
 * Element is here used in the sense of 'element of a sequence",
 * not the XML sense of 'element node'.
 */

public interface ElementPredicate
{
  public boolean isInstancePos (AbstractSequence seq, int ipos);
}

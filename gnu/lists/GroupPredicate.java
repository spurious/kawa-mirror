// Copyright (c) 2002  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A predicate that (only) matches a GROUP_VALUE.
 * If using XML terminology:  only matches element nodes.
 */

public interface GroupPredicate extends NodePredicate
{
  public boolean isInstance(AbstractSequence seq, int ipos, Object groupType);
}

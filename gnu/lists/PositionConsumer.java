// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/**
 * An object that casn be "fed" a TreePosition, and will do something with it.
 * That "something" can be printing it, copy it, filter it - or ignore it.
 */

public interface PositionConsumer
// FIXME rename to PositionList?  Then what about TreePositionList
{
  /**
   * Consume node at current position.
   *
   * The caller may invalidate or change the position after consume returns,
   * so if the consumer wants to save it, it needs to copy it.
   * 
   * @return true if we are interested in more nodes.
   */
  // FIXME rename to add
  public boolean consume(TreePosition position);
}

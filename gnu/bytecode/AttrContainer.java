// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** An interface for objects that (may) contain Attribute objects. */

public interface AttrContainer
{
  public Attribute getAttributes ();
  public void setAttributes (Attribute attribute);
}

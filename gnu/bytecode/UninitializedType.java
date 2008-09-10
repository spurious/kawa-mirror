// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** A pseudo-type used for allocated but uniniialized objects. */

public class UninitializedType extends ObjectType
{
  ClassType ctype;

  public UninitializedType (ClassType ctype)
  {
    super(ctype.getName());
    setSignature(ctype.getSignature());
    this.ctype = ctype;
  }

  public static UninitializedType uninitializedThis (ClassType ctype)
  {
    return new UninitializedType(ctype);
  }

  public static UninitializedType make (ClassType ctype)
  {
    return new UninitializedType(ctype);
  }

  public Type getImplementationType()
  {
    return ctype;
  }

  public String toString()
  {
    return "Uninitialized<" + ctype.getName() + '>';
  }
}

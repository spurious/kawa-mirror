// Copyright (c) 1997  Cygnus Solutions, Inc.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.bytecode;

public class ArrayType extends Type
{
  public Type elements;

  public ArrayType (Type elements)
  {
    super (elements.getName() + "[]", "[" + elements.getSignature());
    this.elements = elements;
  }
}

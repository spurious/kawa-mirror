// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

public class ArrayType extends Type
{
  public Type elements;

  public ArrayType (Type elements)
  {
    super (elements.getName() + "[]", "[" + elements.getSignature());
    this.elements = elements;
  }

  public Class getReflectClass()
  {
    try
      {
	if (reflectClass == null)
	  reflectClass = Class.forName(getSignature());
      }
    catch (java.lang.ClassNotFoundException ex)
      {
      }
    return reflectClass;
  }
}

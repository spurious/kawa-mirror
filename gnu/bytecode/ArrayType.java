// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

public class ArrayType extends ObjectType
{
  public Type elements;

  public ArrayType (Type elements)
  {
    this(elements, elements.getName() + "[]");
  }

  ArrayType (Type elements, String name)
  {
    this_name = name;
    setSignature("[" + elements.getSignature());
    this.elements = elements;
  }

  /** Find or create an ArrayType for the specified element type. */
  public static ArrayType make(Type elements)
  {
    String name = elements.getName() + "[]";
    ArrayType type = (ArrayType) Type.lookupType(name);
    if (type == null || type.elements != elements)
      {
	type = new ArrayType(elements, name);
	mapNameToType.put(name, type);
      }
    return type;
  }

  public Type getComponentType() { return elements; }

  public String getNameOrSignature() { return getSignature(); }

  public int compare(Type other)
  {
    // This is rather coarse .. FIXME
    if (other instanceof ArrayType)
      return -2;
    else if (other.getName().equals("java.lang.Object"))
      return -1;
    else
      return -3;
  }

}

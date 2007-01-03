// Copyright (c) 1997, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.Vector;

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

  public Type getImplementationType()
  {
    Type eltype = elements.getImplementationType();
    return elements == eltype ? this : make(eltype);
  }

  /** Name assumed to end with "[]". */
  static ArrayType make(String name)
  {
    Type elements = Type.getType(name.substring(0, name.length()-2));
    ArrayType array_type = elements.array_type;
    if (array_type == null)
      {
	array_type = new ArrayType(elements, name);
	elements.array_type = array_type;
      }
    return array_type;
  }

  /** Find or create an ArrayType for the specified element type. */
  public static ArrayType make(Type elements)
  {
    ArrayType array_type = elements.array_type;
    if (array_type == null)
      {
	array_type = new ArrayType(elements, elements.getName() + "[]");
	elements.array_type = array_type;
      }
    return array_type;
  }

  public Type getComponentType() { return elements; }

  public String getInternalName() { return getSignature(); }

  public int getMethods (Filter filter, int searchSupers, Vector result,
			 String context)
  {
    if (searchSupers > 0)
      {
        int count = Type.pointer_type.getMethods(filter, 0, result, null);
        if (searchSupers > 1 && filter.select(Type.clone_method))
          {
            if (result != null)
              result.addElement(Type.clone_method);
            count++;
          }
        return count;
      }
    return 0;
  }

  public int compare(Type other)
  {
    if (other == nullType)
      return 1;
    if (other instanceof ArrayType)
      return elements.compare(((ArrayType) other).elements);
    else if (other.getName().equals("java.lang.Object")
	     || other == tostring_type)
      return -1;
    else
      return -3;
  }
}

package gnu.bytecode;

/**
  * Semi-abstract class object reference types.
  * <p>
  * Extended by ClassType and ArrayType. */

public class ObjectType extends Type
{
  protected ObjectType ()
  {
    size = 4;
  }

  ObjectType (String name)
  {
    this_name = name;
    size = 4;
  }

  // Miscellaneous bits:
  final static int ADD_FIELDS_DONE  = 1;
  final static int ADD_METHODS_DONE = 2;
  // A ClassType that we can expect to have a corresponding reflectClass.
  final static int EXISTING_CLASS = 4;
  int flags;

  public String getNameOrSignature()
  {
    return getName();
  }

  /** Get the java.lang.Class object for the representation type. */
  public Class getReflectClass()
  {
    try
      {
	if (reflectClass == null)
	  reflectClass = Class.forName(getNameOrSignature().replace('/', '.'));
        flags |= EXISTING_CLASS;
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        if ((flags & EXISTING_CLASS) != 0)
          throw new RuntimeException("no such class: "+getName());
      }
    return reflectClass;
  }

  public int compare(Type other)
  {
    // Assume this == nullType.
    return other == nullType ? 0 : -1;
  }

  /** Convert an object to a value of this Type.
   * Throw a ClassCastException when this is not possible. */
  public Object coerceFromObject (Object obj)
  {
    if (this == Type.string_type)
      return obj.toString();
    if (obj == null || getReflectClass().isAssignableFrom(obj.getClass()))
      return obj;
    throw new ClassCastException("don't know how to coerce "
				 + obj.getClass().getName() + " to "
				 + getName());
  }

  /** Compile (in given method) cast from Object to this Type. */
  public void emitCoerceFromObject (CodeAttr code)
  {
    if (this == Type.string_type)
      code.emitInvokeVirtual(Type.toString_method);
    else if (this != Type.pointer_type)
      code.emitCheckcast(this);
  }
}

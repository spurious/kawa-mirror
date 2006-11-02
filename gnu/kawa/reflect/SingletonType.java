// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class SingletonType extends ObjectType // implements TypeValue
{
  static final SingletonType instance = new SingletonType("singleton");

  public SingletonType (String name)
  {
    super(name);
  }

  public static final SingletonType getInstance () { return instance; }

  public java.lang.Class getReflectClass()
  {
    return getImplementationType().getReflectClass();
  }

  public Type getImplementationType ()
  {
    return Type.pointer_type;
  }

  public boolean isInstance (Object obj)
  {
    return obj != null && ! (obj instanceof Values);
  }
}

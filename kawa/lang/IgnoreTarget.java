package kawa.lang;

import gnu.bytecode.Type;

public class IgnoreTarget extends Target
{
  public Type getType() { return Type.void_type; }

  public void compileFromStack(Compilation comp, Type stackType)
  {
    if (stackType != Type.void_type)
      comp.getCode().emitPop(1);
  }
}

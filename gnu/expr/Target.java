package gnu.expr;

import gnu.bytecode.Type;

/** This represents where a compiler can put the result of an expression. */

public abstract class Target
{
  public abstract Type getType();

  public abstract void compileFromStack(Compilation comp, Type stackType);

  /** A Target which means that the result is ignored. */
  public static final Target Ignore = new IgnoreTarget();

  /** A Target which means to push an Object on the JVM stack. */
  public static final Target pushObject = new StackTarget(Type.pointer_type);

  public static final Target returnObject = new TailTarget(Type.pointer_type);
}

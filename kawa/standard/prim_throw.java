package kawa.standard;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class prim_throw extends Procedure1 implements Inlineable
{
  public static final prim_throw primitiveThrow = new prim_throw();

  public static void throw_it (Object arg1)
    throws Throwable
  {
    throw ((Throwable) arg1);
  }

  public Object apply1 (Object arg1)
    throws Throwable
  {
    throw_it(arg1);
    return Values.empty;
  }

  private static ClassType javaThrowableType;

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    // Check that exp.args.length == 1.  FIXME!
    gnu.bytecode.CodeAttr code = comp.getCode();
    exp.getArgs()[0].compile(comp, Target.pushObject);
    if (javaThrowableType == null)
      javaThrowableType = new ClassType("java.lang.Throwable");
    code.emitCheckcast(javaThrowableType);
    code.emitThrow();
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.neverReturnsType;
  }
}

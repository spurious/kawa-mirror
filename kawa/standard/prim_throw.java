package kawa.standard;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;

public class prim_throw extends Procedure1 implements Inlineable
{
  public static void throw_it (Object arg1)
  {
    if (arg1 instanceof RuntimeException)
      throw ((RuntimeException) arg1);
    else if (arg1 instanceof Error)
      throw ((Error) arg1);
    else
      throw (new RuntimeException(arg1.toString()));
  }

  public Object apply1 (Object arg1)
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
}

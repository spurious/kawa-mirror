package gnu.expr;
import gnu.bytecode.*;

public class ProcInitializer extends Initializer
{
  LambdaExp proc;

  public ProcInitializer(LambdaExp lexp, Compilation comp)
  {
    field = lexp.allocFieldFor(comp);
    proc = lexp;
    next = comp.initChain;
    comp.initChain = this;
  }

  /** Create and load a ModuleMethod for the given procedure. */
  public static void emitLoadModuleMethod(LambdaExp proc, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    ClassType procClass = Compilation.getMethodProcType(comp.curClass);
    code.emitNew(procClass);
    code.emitDup(1);

    code.emitPushThis();
    code.emitPushInt(proc.getSelectorValue(comp));
    String name = proc.getName();
    if (name == null)
      code.emitPushNull();
    else
      code.emitPushString(name);
    code.emitPushInt(proc.min_args | (proc.max_args << 12));
    Method initModuleMethod = procClass.getDeclaredMethod("<init>", 4);
    code.emitInvokeSpecial(initModuleMethod);
  }

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();

    emitLoadModuleMethod(proc, comp);

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}

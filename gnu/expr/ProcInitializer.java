package gnu.expr;
import gnu.bytecode.*;

public class ProcInitializer extends Initializer
{
  LambdaExp proc;

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();
    code.emitNew(comp.typeModuleMethod);
    code.emitDup(1);
    /*
    PrimProcedure pproc = new PrimProcedure(proc.primMethod, proc);
    comp.applyMethods.addElement(pproc);
    int applyKind = proc.min_args <= 4 && proc.min_args == proc.max_args
      ? proc.min_args : 5;
    comp.applyMethodsCount[applyKind]++;
    */

    code.emitPushThis();
    code.emitPushInt(proc.getSelectorValue(comp));
    String name = proc.getName();
    if (name == null)
      code.emitPushNull();
    else
      code.emitPushString(name);
    code.emitPushInt(proc.min_args | (proc.max_args << 12));
    Type[] constructor_args = { comp.typeModuleBody, Type.int_type,
                                comp.javaStringType, Type.int_type};
    Method initModuleMethod
      = comp.typeModuleMethod.addMethod("<init>", constructor_args,
                                        Type.void_type,
                                        Access.PUBLIC);
    code.emitInvokeSpecial(initModuleMethod);

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}

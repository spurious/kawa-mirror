package gnu.expr;
import gnu.bytecode.*;

public class ProcInitializer extends Initializer
{
  LambdaExp proc;

  public ProcInitializer(LambdaExp lexp, Compilation comp)
  {
    field = lexp.allocFieldFor(comp);
    proc = lexp;
    LambdaExp heapLambda = lexp.getOwningLambda();
    if (heapLambda instanceof ModuleExp && comp.mainLambda.isStatic())
      {
	next = comp.clinitChain;
	comp.clinitChain = this;
      }
    else
      {
	next = heapLambda.initChain;
	heapLambda.initChain = this;
      }
  }

  /** Create and load a ModuleMethod for the given procedure. */
  public static void emitLoadModuleMethod(LambdaExp proc, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    ClassType procClass = Compilation.getMethodProcType(comp.curClass);
    code.emitNew(procClass);
    code.emitDup(1);

    if (comp.method.getStaticFlag())
      code.emitLoad(comp.moduleInstanceVar);
    else
      code.emitPushThis();
    code.emitPushInt(proc.getSelectorValue(comp));
    comp.compileConstant(proc.nameDecl != null ? proc.nameDecl.getSymbol()
			 : proc.getName(),
			 Target.pushObject);
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

    if (proc.properties != null)
      {
	int len = proc.properties.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    Object key = proc.properties[i];
	    if (key != null)
	      {
		Object val = proc.properties[i+1];
		code.emitDup(1);
		comp.compileConstant(key);
                Target target = Target.pushObject;
                if (val instanceof Expression)
                  ((Expression) val).compile(comp, target);
                else
                  comp.compileConstant(val, target);
		Method m = (Compilation.typeProcedure
			    .getDeclaredMethod("setProperty", 2));
		code.emitInvokeVirtual(m);
	      }
	  }
      }

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}

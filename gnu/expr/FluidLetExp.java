// Copyright (c) 1999, 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/**
 * Class used to implement "fluid-let" for Scheme and "let" for Emacs.
 * @author	Per Bothner
 */

public class FluidLetExp extends LetExp
{
  public FluidLetExp (Expression[] i) { super(i); }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    code.pushScope();
    Type result_type = target instanceof IgnoreTarget ? null
	: getType();
    Target ttarg;
    if (result_type == null)
      ttarg = Target.Ignore;
    else if (result_type == Type.pointer_type)
      ttarg = Target.pushObject;
    else
      ttarg = new StackTarget(result_type);
    Variable context = code.addLocal(typeCallContext);
    if (comp.curLambda.isHandlingTailCalls())
      comp.loadCallContext();
    else
      code.emitInvokeStatic(getContextMethod);
    code.emitDup(1);
    code.emitStore(context);
    code.emitDup(1);
    code.emitGetField(fluidBindingsField);
    Variable old_bindings = code.addLocal(typeFluidBinding);
    code.emitDup(1);
    code.emitStore(old_bindings);
    code.enterScope (scope);
    Declaration decl = firstDecl();
    for (int i = 0; i < inits.length; i++, decl = decl.nextDecl())
      {
        decl.allocateVariable(code);
	inits[i].compile(comp, Target.pushObject);
	decl.base.load(comp);
	code.emitInvokeStatic(makeFluidBindingMethod);
	code.emitDup(1);
	code.emitStore(decl.getVariable());
      }
    code.emitInvokeVirtual(setFluidsMethod);
    code.emitTryStart(true, result_type);
    body.compileWithPosition(comp, ttarg);
    code.emitTryEnd();
    code.emitFinallyStart();
    code.emitLoad(context);
    code.emitLoad(old_bindings);
    code.emitInvokeVirtual(resetFluidsMethod);
    code.emitTryCatchEnd();
    code.popScope();
    code.popScope();
    if (result_type != null)
      target.compileFromStack(comp, result_type);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkFluidLetExp(this);
  }

  static ClassType typeCallContext = ClassType.make("gnu.mapping.CallContext");
  static Method getContextMethod
    = typeCallContext.addMethod("getInstance",
				Type.typeArray0, typeCallContext,
				Access.STATIC|Access.PUBLIC);
  public static ClassType typeFluidBinding
    = ClassType.make("gnu.mapping.FluidBinding");
  static Field fluidBindingsField
    = typeCallContext.addField("fluidBindings", typeFluidBinding,
			  Access.STATIC|Access.PUBLIC);
  public static Field valueField
    = typeFluidBinding.addField("value", Type.pointer_type, Access.PUBLIC);
  static Method setFluidsMethod;
  static Method resetFluidsMethod;
  static Method makeFluidBindingMethod;
  static
    {
      Type[] args = { typeFluidBinding };
      setFluidsMethod = typeCallContext.addMethod
	("setFluids", args, Type.void_type, Access.PUBLIC);
      resetFluidsMethod = typeCallContext.addMethod
	("resetFluids", args, Type.void_type, Access.PUBLIC);
      args = new Type[3];
      args[0] = typeFluidBinding;
      args[1] = Type.pointer_type;
      args[2] = ClassType.make("gnu.mapping.Symbol");
      makeFluidBindingMethod = typeFluidBinding.addMethod
	("make", args, typeFluidBinding, Access.PUBLIC|Access.STATIC);
    }
}

// Copyright (c) 1999, 2000  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.OutPort;

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
    code.enterScope(getVarScope());
    Variable[] save = new Variable[inits.length];
    
    Declaration decl = firstDecl();
    doInits(decl, 0, save, comp);
    code.emitTryStart(true, result_type);
    body.compileWithPosition(comp, ttarg);
    code.emitTryEnd();
    code.emitFinallyStart();

    
    for (int i = 0; i < inits.length; i++, decl = decl.nextDecl())
      {
	decl.load(comp);
	code.emitLoad(save[i]);
	code.emitInvokeVirtual(comp.typeLocation
			       .getDeclaredMethod("setRestore", 1));
	
      }
    code.emitTryCatchEnd();
    code.popScope();
    code.popScope();
    if (result_type != null)
      target.compileFromStack(comp, result_type);
  }

  private void doInits (Declaration decl, int i, Variable[] save,
			Compilation comp)
  {
    if (i >= inits.length)
      return;
    CodeAttr code = comp.getCode();
    save[i] = code.addLocal(Type.pointer_type);
    decl.allocateVariable(code);
    decl.base.load(comp);
    code.emitDup();
    code.emitStore(decl.getVariable());
    inits[i].compile(comp, Target.pushObject);
    doInits(decl.nextDecl(), i+1, save, comp);
    code.emitInvokeVirtual(Compilation.typeLocation
			   .getDeclaredMethod("setWithSave", 1));
    code.emitStore(save[i]);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkFluidLetExp(this);
  }

  public void print (OutPort out)
  {
    print(out, "(FluidLet", ")");
  }
}

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ObjectExp extends ClassExp
{
  public ObjectExp ()
  {
    type = null;
    // Make sure we actually generate a class.
    setCanRead(true);
  }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkObjectExp(this);
  }

  public void compile (Compilation comp, Target target)
  {
    ClassType new_class = compile (comp);
    CodeAttr code = comp.getCode();
    code.emitNew(type);
    code.emitDup(1);
    Method init = comp.getConstructor(type, this);
    if (closureEnvField != null)
      {
	LambdaExp caller = outerLambda();
	Variable closureEnv =
	  ! Compilation.usingTailCalls ? getOwningLambda().heapFrame
	  : caller.heapFrame != null ? caller.heapFrame	: caller.closureEnv;
	code.emitLoad(closureEnv);
      }
    code.emitInvokeSpecial(init);

    target.compileFromStack(comp, getCompiledClassType(comp));
  }

}

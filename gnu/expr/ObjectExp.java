package gnu.expr;
import gnu.bytecode.*;

public class ObjectExp extends ClassExp
{
  public ObjectExp ()
  {
    super(true, new ClassType());
  }

  protected Type calculateType() { return compiledType; }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitObjectExp(this, d);
  }

  public void compile (Compilation comp, Target target)
  {
    compileMembers(comp);
    CodeAttr code = comp.getCode();
    code.emitNew(compiledType);
    code.emitDup(1);
    Method init = Compilation.getConstructor(compiledType, this);
    if (closureEnvField != null)
      {
	LambdaExp caller = outerLambda();
	Variable closureEnv =
	  Compilation.defaultCallConvention < Compilation.CALL_WITH_CONSUMER ? getOwningLambda().heapFrame
	  : caller.heapFrame != null ? caller.heapFrame	: caller.closureEnv;
	if (closureEnv == null)
	  code.emitPushThis();
	else
	  code.emitLoad(closureEnv);
      }
    code.emitInvokeSpecial(init);

    target.compileFromStack(comp, getCompiledClassType(comp));
  }

}

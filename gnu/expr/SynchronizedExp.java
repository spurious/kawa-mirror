package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class SynchronizedExp extends Expression
{
  Expression object;
  Expression body;

  public SynchronizedExp (Expression object, Expression body)
  {
    this.object = object;
    this.body = body;
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object value = object.eval(ctx);
    Object result;
    synchronized (value)
      {
	result = body.eval(ctx);
      }
    ctx.writeValue(result);
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    object.compile (comp, Target.pushObject);
    code.emitDup(1);
    Scope scope = code.pushScope();
    Variable objvar = scope.addVariable(code, Type.pointer_type, null);
    code.emitStore(objvar); 
    code.emitMonitorEnter();
    code.emitTryStart(false,
		      (target instanceof IgnoreTarget
		       || target instanceof ConsumerTarget) ? null
		      : target.getType());

    body.compileWithPosition(comp, target); 
    code.emitLoad(objvar);
    code.emitMonitorExit();
    code.emitTryEnd();
    code.emitCatchStart(null);
    code.emitLoad(objvar);
    code.emitMonitorExit(); 
    code.emitThrow(); 
    code.emitCatchEnd();
    code.emitTryCatchEnd();
    code.popScope();
 }

  protected Expression walk (ExpWalker walker)
  {
    return walker.walkSynchronizedExp(this);
  }

  protected void walkChildren(ExpWalker walker)
  {
    object = walker.walk(object);
    if (walker.exitValue == null)
      body = walker.walk(body);
  }

  public void print (OutPort ps)
  {
    ps.print("(Synchronized ");
    object.print(ps);
    ps.print(" ");
    body.print(ps);
    ps.print(")");
  }
}

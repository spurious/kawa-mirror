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

  public Object eval (Environment env)
  {
    Object value = object.eval(env);
    synchronized (value)
      {
	return body.eval(env);
      }
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
		      target instanceof IgnoreTarget ? null
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
    object = object.walk(walker);
    if (walker.exitValue == null)
      body = body.walk(walker);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print("(#%synchronized ");
    object.print(ps);
    ps.print(" ");
    body.print(ps);
    ps.print(")");
  }
}

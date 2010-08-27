package gnu.kawa.functions;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class CallCC extends MethodProc implements Inlineable
{
  public static final CallCC callcc = new CallCC();

  CallCC ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileMisc:validateApplyCallCC");
  }

  public int numArgs() { return 0x1001; }

  public int match1 (Object proc, CallContext ctx)
  {
    if (! (proc instanceof Procedure))
      return NO_MATCH_BAD_TYPE;
    return super.match1(proc, ctx);
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Procedure proc = (Procedure) ctx.value1;
    Continuation cont = new Continuation(ctx);
    proc.check1(cont, ctx);
    proc = ctx.proc;
    ctx.proc = null;
    try
      {
	proc.apply(ctx);
	ctx.runUntilDone();
	cont.invoked = true;
      }
    catch (Throwable ex)
      {
        Continuation.handleException$X(ex, cont, ctx);
      }
  }

  /*
  public void apply (CallContext stack)
  {
    kawa.lang.Continuation cont = new Continuation ();
    cont.frame = stack.proc;
    cont.pc = stack.pc;
    stack.value = cont;
  }
  */

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    CompileMisc.compileCallCC(exp, comp, target, this);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}

/*
class Continuation extends MethodProc
{
  Procedure frame;
  int pc;

  public void apply (CallContext stack)
  {
    Object result = Values.make(stack.args);
    stack.pc = pc;
    stack.proc = frame;
    stack.result = result;
  }
}
*/

/** A hack to simplify inlining compilation calls. */

class CompileTimeContinuation extends ProcedureN implements Inlineable
{
  Target blockTarget;
  ExitableBlock exitableBlock;

  public Object applyN (Object[] args) throws Throwable
  {
    throw new Error("internal error");
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    boolean noStack = (blockTarget instanceof IgnoreTarget
                       || blockTarget instanceof ConsumerTarget);
    Type typeNeeded = noStack ? null : target.getType();
    if (noStack || nargs == 1)
      {
        for (int i = 0;  i < nargs;  i++)
          args[i].compileWithPosition(comp, blockTarget);
      }
    else
      {
        AppendValues app = AppendValues.appendValues;
        app.compile(new ApplyExp(app, args), comp, blockTarget);
      }
    exitableBlock.exit();
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return Type.neverReturnsType;
  }
}

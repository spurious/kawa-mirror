package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.functions.AppendValues;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class callcc extends MethodProc implements CanInline, Inlineable
{
  public static final callcc callcc = new callcc();

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

  /** If we can inline, return LambdaExp for first arg; otherwise null. */
  private LambdaExp canInline (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    Expression arg0;
    if (args.length == 1 && (arg0 = args[0]) instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) arg0;
        if (lexp.min_args == 1 && lexp.max_args == 1
            && ! lexp.firstDecl().getCanWrite())
          {
            return lexp;
          }
      }
    return null;
  }

  public static final ClassType typeContinuation =
    ClassType.make("kawa.lang.Continuation");

  public Expression inline (ApplyExp exp, InlineCalls walker,
                            boolean argsInlined)
  {
    LambdaExp lexp = canInline(exp);
    if (lexp != null)
      {
	lexp.setInlineOnly(true);
	lexp.returnContinuation = exp;
        lexp.inlineHome = walker.getCurrentLambda();
        Declaration contDecl = lexp.firstDecl();
        if (! contDecl.getFlag(Declaration.TYPE_SPECIFIED))
          contDecl.setType(typeContinuation);
      }
    exp.walkArgs(walker, argsInlined);
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    LambdaExp lambda = canInline(exp);
    if (lambda == null)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    CodeAttr code = comp.getCode();
    final Declaration param = lambda.firstDecl();
    if (param.isSimple() && ! param.getCanRead() && ! param.getCanWrite())
      {
        CompileTimeContinuation contProxy = new CompileTimeContinuation();
        Type rtype = target instanceof StackTarget ? target.getType() : null;
        boolean runFinallyBlocks
          = ExitThroughFinallyChecker.check(param, lambda.body);
        ExitableBlock bl = code.startExitableBlock(rtype, runFinallyBlocks);
        contProxy.exitableBlock = bl;
        contProxy.blockTarget = target;
        param.setValue(new QuoteExp(contProxy));
        lambda.body.compile(comp, target);
        code.endExitableBlock();
        return;
      }

    Scope sc = code.pushScope();
    Variable contVar = sc.addVariable(code, typeContinuation, null);
    Declaration contDecl = new Declaration(contVar);
    code.emitNew(typeContinuation);
    code.emitDup(typeContinuation);
    comp.loadCallContext();
    code.emitInvokeSpecial(typeContinuation.getDeclaredMethod("<init>", 1));
    code.emitStore(contVar);
    code.emitTryStart(false, target instanceof IgnoreTarget || target instanceof ConsumerTarget ? null : Type.objectType);
    ApplyExp app = new ApplyExp(lambda, new Expression[] { new ReferenceExp(contDecl) });
    app.compile(comp, target);
    // Emit: cont.invoked = true
    if (code.reachableHere())
      {
        code.emitLoad(contVar);
        code.emitPushInt(1);
        code.emitPutField(typeContinuation.getField("invoked"));
      }
    code.emitTryEnd();

    // Emit: catch (Throwable ex) { handleException$(ex, cont, ctx); }
    code.emitCatchStart(null);
    code.emitLoad(contVar);
    if (target instanceof ConsumerTarget)
      {
        comp.loadCallContext();
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException$X", 3);
        code.emitInvokeStatic(handleMethod);
      }
    else
      {
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException", 2);
        code.emitInvokeStatic(handleMethod);
        target.compileFromStack(comp, Type.objectType);
      }
    code.emitCatchEnd();

    code.emitTryCatchEnd();
    code.popScope();
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }

  /** An ExpWalker class to check if callcc exits through a try-finally. */
  static class ExitThroughFinallyChecker extends ExpWalker
  {
    TryExp currentTry = null;
    Declaration decl;

    /** Does decl appear in body nested inside a try-finally? */
    public static boolean check (Declaration decl, Expression body)
    {
      ExitThroughFinallyChecker walker = new ExitThroughFinallyChecker();
      walker.decl = decl;
      walker.walk(body);
      return walker.exitValue != null;
    }

    protected Expression walkReferenceExp (ReferenceExp exp)
    {
      if (decl == exp.getBinding() && currentTry != null)
        exitValue = Boolean.TRUE;
      return exp;
    }

    protected Expression walkTryExp (TryExp exp)
    {
      TryExp saveTry = currentTry;
      if (exp.getFinallyClause() != null)
        currentTry = exp;
      walkExpression(exp);
      currentTry = saveTry;
      return exp;
    }
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

package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static String evalFunctionName = "atEvalLevel";

  public static void eval (Object sexpr, Environment env, CallContext ctx)
    throws Throwable
  {
    PairWithPosition body;
    if (sexpr instanceof PairWithPosition)
      body = new PairWithPosition((PairWithPosition) sexpr,
				  sexpr, LList.Empty);
    else
      {
	body = new PairWithPosition(sexpr, LList.Empty);
	body.setFile("<eval>");
      }
    evalBody(body, env, new SourceMessages(), ctx);
  }

  public static Object evalBody (Object body, Environment env,
				 SourceMessages messages)
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = ctx.vstack;
	ctx.values = Values.noArgs;
	evalBody(body, env, messages, ctx);
	return Values.make((gnu.lists.TreeList) ctx.vstack);
      }
    finally
      {
	ctx.vstack.clear();
	ctx.consumer = save;
      }
  }

  public static Object eval (Object sexpr, Environment env)
        throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    try
      {
	ctx.consumer = ctx.vstack;
	ctx.values = Values.noArgs;
	eval(sexpr, env, ctx);
	return Values.make((gnu.lists.TreeList) ctx.vstack);
      }
    finally
      {
	ctx.vstack.clear();
	ctx.consumer = save;
      }
  }

  public static void evalBody (Object body, Environment env,
			       SourceMessages messages, CallContext ctx)
    throws Throwable
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	Translator tr = new Translator (env, messages);
	ModuleExp mod = gnu.kawa.lispexpr.LispInterpreter.makeModuleExp(body, tr);
	if (body instanceof PairWithPosition)
	  mod.setFile(((PairWithPosition) body).getFile());
	mod.setName (evalFunctionName);
	ModuleExp.evalModule(env, ctx, tr);
	if (messages.seenErrors())
	  throw new RuntimeException("invalid syntax in eval form:\n"
				     + messages.toString(20));
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public Object apply1 (Object arg1)
    throws Throwable
  {
    return eval (arg1, Environment.user ());
  }

  public Object apply2 (Object arg1, Object arg2)
    throws Throwable
  {
    return eval (arg1, (Environment) arg2);
  }

  public void apply (CallContext ctx)
    throws Throwable
  {
    Procedure.checkArgCount(this, ctx.count);
    Object exp = ctx.getNextArg();
    Environment env;
    env = (Environment) ctx.getNextArg(null);
    if (env == null)
      env = Environment.user();
    ctx.lastArg();
    eval(exp, env, ctx);
  }
}

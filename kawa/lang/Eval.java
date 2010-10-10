// Copyright (C) 2005, 2010 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  public static final Eval eval = new Eval();
  static { eval.setName("eval"); }

  final static String evalFunctionName = "atEvalLevel$";

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
    int oldIndex = ctx.startFromContext();
    try
      {
	evalBody(body, env, messages, ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  public static Object eval (Object sexpr, Environment env)
        throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    int oldIndex = ctx.startFromContext();
    try
      {
	eval(sexpr, env, ctx);
	return ctx.getFromContext(oldIndex);
      }
    catch (Throwable ex)
      { 
	ctx.cleanupFromContext(oldIndex);
	throw ex;
      }
  }

  public static void evalBody (Object body, Environment env,
			       SourceMessages messages, CallContext ctx)
    throws Throwable
  {
    Language language = Language.getDefaultLanguage();
    Environment saveGlobalEnv = Environment.getCurrent();
    try
      {
 	if (env != saveGlobalEnv)
	  Environment.setCurrent(env);
	Translator tr = new Translator(language, messages,
                                       NameLookup.getInstance(env, language));
        tr.immediate = true;
        // The state value BODY_PARSED-1 causes require#importDefinitions
        // to do the right thing.
        tr.setState(Compilation.BODY_PARSED-1);
        tr.setSharedModuleDefs(true);
	ModuleExp mod = tr.pushNewModule((String) null);
        Compilation saveComp = Compilation.setSaveCurrent(tr);
        try
          {
            int first = tr.formStack.size();
            tr.scanBody(body, mod, false);
            tr.firstForm = first;
            tr.finishModule(mod);
          }
        finally
          {
            Compilation.restoreCurrent(saveComp);
          }

	if (body instanceof PairWithPosition)
	  mod.setFile(((PairWithPosition) body).getFileName());
	mod.setName(evalFunctionName + (++ModuleExp.interactiveCounter));
	ModuleExp.evalModule(env, ctx, tr, null, null);
	if (messages.seenErrors())
	  throw new RuntimeException("invalid syntax in eval form:\n"
				     + messages.toString(20));
      }
    finally
      {
	if (env != saveGlobalEnv)
	  Environment.setCurrent(saveGlobalEnv);
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

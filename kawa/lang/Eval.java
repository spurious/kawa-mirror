package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.text.SourceMessages;
import gnu.lists.*;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static String evalFunctionName = "atEvalLevel";

  public static Object eval (Object sexpr, Environment env)
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
    return evalBody(body, env, new SourceMessages());
  }

  public static Object evalBody (Object body, Environment env,
				 SourceMessages messages)
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	Translator tr = new Translator (env, messages);
	ModuleExp mod = kawa.standard.Scheme.makeModuleExp(body, tr);
	if (body instanceof PairWithPosition)
	  mod.setFile(((PairWithPosition) body).getFile());
	mod.setName (evalFunctionName);
	if (messages.seenErrors())
	  throw new RuntimeException("invalid syntax in eval form:\n"
				     + messages.toString(20));
	return mod.evalModule (env);
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public Object apply1 (Object arg1)
  {
    return eval (arg1, Environment.user ());
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return eval (arg1, (Environment) arg2);
  }
}

package kawa.lang;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static String evalFunctionName = "atEvalLevel";

  public static Object eval (Object sexpr, Environment env)
  {
    PairWithPosition body = new PairWithPosition(sexpr, List.Empty);
    body.setFile("<eval>");
    return evalBody(body, env);
  }

  public static Object evalBody (Object body, Environment env)
  {
    Environment orig_env = Environment.getCurrent();
    try
      {
	if (env != orig_env)
	  Environment.setCurrent(env);
	Translator tr = new Translator (env);
	ModuleExp mod = new ModuleExp (body, tr);
	mod.setName (evalFunctionName);
	if (tr.errors > 0)
	    throw new GenericError ("syntax errors during eval");
	return mod.evalModule (env);
      }
    finally
      {
	if (env != orig_env)
	  Environment.setCurrent(orig_env);
      }
  }

  public Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return eval (arg1, Environment.user ());
  }

  public Object apply2 (Object arg1, Object arg2)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return eval (arg1, (Environment) arg2);
  }
}

package kawa.lang;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
{
  /** The name of the formal parameter for the incoming Environment. */
  static private String env_formal = Symbol.makeUninterned ("theEnvironment");
  /** The formal parameter list of a ModuleBody. */
  static public Object formals = new Pair (env_formal, List.Empty);

  /** True if the body is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a lambda (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile;

  public ModuleExp (Object body, Translator tr, String filename)
  {
    super (formals, body, tr);
    lookup(env_formal).setType(Compilation.scmEnvironmentType);
    setFile (filename);
  }

  public final Object eval_module (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    if (! mustCompile) // optimization - don't generate unneeded Class.
      return body.eval (env);
    Environment orig_env = Environment.getCurrent();
    try
      {
	Environment.setCurrent(env);
	return ((ModuleBody) eval (env)).run (env);
      }
    finally
      {
	Environment.setCurrent(orig_env);
      }
  }
}

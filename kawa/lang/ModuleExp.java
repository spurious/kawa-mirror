package kawa.lang;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
{
  /** The name of the formal parameter for the incoming Environment. */
  static private Symbol env_formal = Symbol.makeUninterned ("theEnvironment");
  /** The formal parameter list of a ModuleBody. */
  static public Object formals = new Pair (env_formal, List.Empty);

  public ModuleExp (List body, Translator tr, String filename)
  {
    super (formals, body, tr);
    lookup (env_formal).type = Compilation.scmEnvironmentType;
    setFile (filename);
  }

  public final Object eval_module (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    if (!hasNestedScopes) // optimization - don't generate unneeded Class.
      return body.eval (env);
    return ((ModuleBody) eval (env)).run (env);
  }
}

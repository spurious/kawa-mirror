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

  public ModuleExp (List body, Environment env)
  {
    super (formals, body, env.interpreter());
    lookup (env_formal).type = Compilation.scmEnvironmentType;
  }

}

package kawa.lang;

/**
 * Abstract class for the dummy top-level function of a module. */

public abstract class ModuleBody extends Procedure0
{
  /** The name of the formal parameter for the incoming Environment. */
  static private Symbol env_formal = Symbol.makeUninterned ("theEnvironment");
  /** The formal parameter list of a ModuleBody. */
  static public Object formals = new Pair (env_formal, List.Empty);

  public Object apply0 ()
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return run (Environment.current ());
  }

  public abstract Object run (Environment env)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol;

}

package kawa.lang;

/**
 * An environment contains (name->value) bindings.
 * @author	Per Bothner
 */

public class Environment
{
  kawa.lang.Interpreter interp;
  /** The ScopeExp that create this Environment. */
  ScopeExp scope;
  /** The actual frame with the values. */
  Object[] values;
  /** The lexically surrounding environment. */
  Environment outer;

  // Obsolete (soon)
  public Environment (kawa.lang.Interpreter i)
  {
    interp = i;
  }

  public Interpreter getInterpreter ()
  {
    return interp;
  }

  /**
   * Create a new extended Environment.
   * @param vals the fame of the new values
   * @param sc the Expression that created the new Environment
   * @param env the next Environment to search
   */
  public Environment (Object[] vals, ScopeExp sc, Environment env)
  {
    interp = env.interp;
    values = vals;
    scope = sc;
    outer = env;
  }

  /**
   * Search for a variable binding by name.
   * Searches up the Environment chain, and also the global bindings.
   * @param sym the name of the binding to search for
   * @return the value of the binding, or null if not found
   */
  /*
  Object lookup (Symbol sym)
  {
    Environment env = this;
    while (env != null && env.scope != null)
      {
	Declaration decl = env.scope.lookup (sym);
	if (decl != null)
	  return env.values[decl.offset];
      }
    return interp.globals.get (sym.toString ());
  */

  /**
   * Evaluate an expression in this Environment.
   */
  final public Object
  eval (Expression expr)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    return expr.eval (this);
  }
}



package kawa.lang;

/**
 * Class used to implement Scheme lambda expressions.
 * @author	Per Bothner
 */

public class LambdaExp extends ScopeExp
{
  Expression body;
  int min_args;
  // Maximum number of actual arguments;  -1 if variable.
  int max_args;
  public final boolean variable_args () { return max_args < 0; }

  public LambdaExp (Declaration[] params, Expression bod,
			  int min, int max)
  {
    decls = params;  body = bod;  min_args = min;  max_args = max;
  }

  /**
   * Higher-level constructor, that does the re-writing.
   * @param formals the formal parameter list (or symbol)
   * @param body the body of the procedure
   * @param interp the (Scheme) interpreter
   */
  public LambdaExp (Object formals, Object body, Interpreter interp)
    throws WrongArguments
  {
    /* Count formals, while checking that the syntax is OK. */
    Object bindings = formals;
    for (; bindings instanceof Pair; min_args++)
      bindings = ((Pair)bindings).cdr;
    if (bindings == List.Empty)
      max_args = min_args;
    else if (bindings instanceof Symbol)
      max_args = -1;
    else
      throw new WrongArguments ("lambda", 2,
				"(lambda formals body) [invalid formals]");

    decls = new Declaration[min_args + (max_args < 0 ? 1 : 0)];
    bindings = formals;
    while (bindings instanceof Pair)
      {
	Pair bind_pair = (Pair) bindings;
	add_decl ((Symbol) bind_pair.car);
	bindings = bind_pair.cdr;
      }
    if (bindings instanceof Symbol)
      {
	add_decl ((Symbol) bindings);
      }
    push (interp);
    this.body = interp.rewrite_body (body);
    pop (interp);
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    return new LambdaProcedure (this, env);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print("(#%lambda ... ");
    body.print (ps);
    ps.print(")");
  }
}

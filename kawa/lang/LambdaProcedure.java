package kawa.lang;

/**
 * Interpreted Procedure resulting from a lambda expression.
 * @author	Per Bothner
 */

public class LambdaProcedure extends ProcedureN
{
  /* The captured lexical context. */
  Environment environment;
  /* The lamda expression that evaluated to this. */
  LambdaExp lexpr;

  public LambdaProcedure (LambdaExp lexp, Environment env)
  {
    super ("<lambda>");
    lexpr = lexp;  environment = env;
  }

  public Object applyN (Object[] args)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (args.length < lexpr.min_args
	|| (lexpr.max_args >= 0 && args.length > lexpr.max_args))
      throw new WrongArguments(this.name,lexpr.min_args,"(?)");
    Object[] frame;
    if (lexpr.max_args < 0) // Variable-arity procedure
      {
	frame = new Object[lexpr.min_args + 1];
	System.arraycopy (args, 0, frame, 0, lexpr.min_args);
	// The last parameter gets a list of the remaining arguments.
	Object list = Interpreter.nullObject;
	for (int i = args.length;  --i >= lexpr.min_args; )
	  list = new pair (args[i], list);
	frame[lexpr.min_args] = list;
      }
    else
      frame = args;
    return lexpr.body.eval (new Environment (frame, lexpr, environment));
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<kawa.lang.LambdaProcedure>");
  }
}

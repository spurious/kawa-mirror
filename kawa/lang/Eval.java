package kawa.lang;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static Symbol evalFunctionName = Symbol.make ("atEvalLevel");

  public static Object eval (LambdaExp lexp, Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Interpreter interpreter = env.interpreter ();
    int save_errors = interpreter.errors;
    lexp.setName (evalFunctionName);
    lexp.filename = "<eval>";
    if (interpreter.errors > save_errors)
      throw new GenericError ("syntax errors during eval");
    return lexp.eval_module (env);
  }

  public static Object eval (Object sexpr, Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    return eval (new ModuleExp (new Pair (sexpr, List.Empty), env), env);
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

package kawa.lang;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static Symbol evalFunctionName = Symbol.make ("atEvalLevel");

  public Object eval (Object sexpr, Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Interpreter interpreter = env.interpreter ();
    int save_errors = interpreter.errors;
    sexpr = new Pair (sexpr, List.Empty);
    LambdaExp lexp = new LambdaExp (ModuleBody.formals, sexpr, interpreter);
    lexp.setName (evalFunctionName);
    lexp.filename = "<eval>";
    if (interpreter.errors > save_errors)
      throw new GenericError ("syntax errors during eval");
    return lexp.eval_module (env);
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

package kawa.lang;

/* This implements the R5RS "eval" procedure. */

public class Eval extends Procedure1or2
{
  final static Symbol evalFunctionName = Symbol.make ("atEvalLevel");

  private static Object eval (ModuleExp mod, Translator tr,
			      Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    mod.setName (evalFunctionName);
    if (tr.errors > 0)
      throw new GenericError ("syntax errors during eval");
    return mod.eval_module (env);
  }

  public static Object eval (Object sexpr, Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    Translator tr = new Translator (env);
    ModuleExp mod = new ModuleExp (new Pair (sexpr, List.Empty), tr, "<eval>");
    return eval (mod, tr, env);
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

package kawa.lang;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression implements Printable
{
  abstract public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError;

  abstract public void print (java.io.PrintStream ps);

  abstract public void compile (Compilation comp, boolean ignore_result);

}

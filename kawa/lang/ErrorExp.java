package kawa.lang;

/**
 * Class used to mark an erroreous expression
 * @author	Per Bothner
 */

public class ErrorExp extends Expression
{
  String message;
  public ErrorExp (String message)
  {
    this.message = message;
  }

  public Object eval (Environment env)
       throws UnboundSymbol, WrongArguments, WrongType, GenericError
  {
    // Should not happen
    throw new GenericError ("evaluated erroreous expression: " + message);
  }

  public void print (java.io.PrintStream ps)
  {
    ps.print ("(%error ");
    ps.print (message);
    ps.print (")");
  }
}

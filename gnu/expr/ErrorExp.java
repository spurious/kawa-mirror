package gnu.expr;
import gnu.mapping.*;

/**
 * Class used to mark an erroneous expression
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
  {
    // Should not happen
    throw new RuntimeException ("evaluated erroneous expression: " + message);
  }

  public void print (java.io.PrintWriter ps)
  {
    ps.print ("(%error ");
    ps.print (message);
    ps.print (")");
  }

  public void compile (Compilation comp, Target target)
  {
    // Should never happen!
  }
}

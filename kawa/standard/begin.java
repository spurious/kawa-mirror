package kawa.standard;
import kawa.lang.*;

/**
 * Implement the re-writer for the "begin" primitive.
 * @author	Per Bothner
 */

public class begin extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    return interp.rewrite_body (obj);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin begin>");
  }
}

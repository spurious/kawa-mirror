package kawa.lang;

/**
 * The Syntax transformer that re-writes the "quote" Scheme primitive.
 * @author	Per Bothner
 */

public class Quote extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (1);

  public Expression rewrite (Object obj, Interpreter interp)
       throws kawa.lang.WrongArguments
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      throw new kawa.lang.WrongArguments("quote",1,"(quote value)");
    return new QuoteExp (match[0]);
  }

  public void print(java.io.PrintStream ps)
  {
    ps.print("#<builtin quote>");
  }
}

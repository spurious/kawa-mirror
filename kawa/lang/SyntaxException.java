package kawa.lang;

/** Used to signal a non-recoverable (i.e. fatal) syntax error. */

public class SyntaxException extends Exception
{
  Lexer lexer;

  public SyntaxException(Lexer lexer)
  {
    this.lexer = lexer;
  }

  public void printAll(java.io.PrintWriter out, int max)
  {
    SourceError.printAll(lexer.firstError, out, max);
  }
}

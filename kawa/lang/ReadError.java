package kawa.lang;

/**
 * A syntax error during read.
 */

public class ReadError extends SyntaxError
{
  ReadError (InPort inport, String message)
  {
    super (message);
  }
}

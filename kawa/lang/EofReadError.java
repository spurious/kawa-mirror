package kawa.lang;

/**
 * An unexpected EOF error during read.
 */

public class EofReadError extends ReadError
{
  public EofReadError (InPort inport, String message)
  {
    super (inport, message);
  }
}

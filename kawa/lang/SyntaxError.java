package kawa.lang;

public class SyntaxError extends Exception
{
  String message;

  SyntaxError (String message)
  {
    this.message = message;
  }

  public String toString ()
  {
    return message;
  }
}

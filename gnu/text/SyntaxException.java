package gnu.text;

/** Used to signal a non-recoverable (i.e. fatal) syntax error. */

public class SyntaxException extends Exception
{
  SourceMessages messages;

  public SyntaxException(SourceMessages messages)
  {
    this.messages = messages;
  }

  public SourceMessages getMessages () { return messages; }

  public void printAll(java.io.PrintWriter out, int max)
  {
    messages.printAll(out, max);
  }
}

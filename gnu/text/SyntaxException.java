package gnu.text;

/** Used to signal a non-recoverable (i.e. fatal) syntax error. */

public class SyntaxException extends Exception
{
  String header;
  SourceMessages messages;

  public SyntaxException(SourceMessages messages)
  {
    this.messages = messages;
  }

  public final String getHeader() { return header; }
  public final void setHeader(String header) { this.header = header; }

  public SourceMessages getMessages () { return messages; }

  public void printAll(java.io.PrintWriter out, int max)
  {
    if (header != null)
      out.println(header);
    messages.printAll(out, max);
  }

  public void clear()
  {
    messages.clear();
  }
}

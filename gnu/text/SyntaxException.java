package gnu.text;

/** Used to signal a non-recoverable (fatal) syntax error. */

public class SyntaxException extends Exception
{
  String header;
  SourceMessages messages;

  public SyntaxException(SourceMessages messages)
  {
    this.messages = messages;
  }

  public SyntaxException(String header, SourceMessages messages)
  {
    this.header = header;
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

  public int maxToPrint = 10;

  public String getMessage ()
  {
    StringBuffer buffer = new StringBuffer ();
    if (header != null)
      buffer.append(header);
    int max = maxToPrint;
    for (SourceError err = messages.firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	buffer.append('\n');
	buffer.append(err);
      }
    return buffer.toString();
  }
}

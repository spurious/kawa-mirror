package gnu.text;

/** Represents an error message from processing a "source" file. */

public class SourceError
{
  public SourceError next;

  /** The seriousness of the error - one of 'w' (for warning),
   * 'e' (for error), or 'f' (for fatal error). */
  public char severity;

  public String filename;

  /** The (1-origin) location of the error. */
  public int line;
  public int column;

  public String message;

  public SourceError(char severity, String filename, int line, int column, 
		     String message)
  {
    this.severity = severity;
    this.filename = filename;
    this.line = line;
    this.column = column;
    this.message = message;
  }


  public SourceError(LineBufferedReader port, char severity, String message)
  {
    this(severity, port.getName(),
	 port.getLineNumber() + 1, port.getColumnNumber(),
	 message);
    if (column >= 0)
      column++;
  }

  public String toString()
  {
    StringBuffer buffer = new StringBuffer ();
    buffer.append (filename == null ? "<unknown>" : filename);
    buffer.append (':');
    buffer.append (line);
    if (column > 0)
      {
	buffer.append (':');
	buffer.append (column);
      }
    buffer.append (": ");
    if (severity == 'w')
      buffer.append("warning - ");
    buffer.append (message);
    return buffer.toString ();
  }

  public void print(java.io.PrintWriter out)
  {
    out.print(this);
  }

  static void printAll(SourceError err, java.io.PrintWriter out, int max)
  {
    for (; err != null && --max >= 0;  err = err.next)
      {
	out.println(err);
      }
  }
}

package gnu.mapping;
import java.io.*;

/** A class to compensate from System.in blocking other threads. */

public class SysInPort extends TtyInPort
{
  InputStream inStream;

  public SysInPort (InputStream inStream, String name, OutPort tie)
  {
    super(inStream, name, tie);
    this.inStream = inStream;
  }

  public int fill (char[] buffer, int off, int len) throws java.io.IOException
  {
    /* This explanation and work-around comes from jacl 1.0's Shell.java:
     *
     * On Unix platforms, System.in.read() will block the delivery of
     * of AWT events. We must make sure System.in.available() is larger
     * than zero before attempting to read from System.in. Since
     * there is no asynchronous IO in Java, we must poll the System.in
     * every 100 milliseconds.
     */

    if (! in.ready())
      {
	try
	  {
	    while (inStream.available() == 0)
	      Thread.currentThread().sleep(100);
	  }
	catch (java.lang.InterruptedException ex)
	  {
	  }
	catch (java.io.IOException ex)
	  {
	    // available or ready does not seem to be supported or working.
	  }
      }


    // It would be more efficient to just do super.fill(buffer, off, len).
    // Unfortunately, that may hang.  One reported problem is when running
    // Kawa inside a shell or inferior-lisp window in ntemacs on Windows NT.
    // Ntemacs uses pipes to communicate with the inferior shell (because
    // pseudo-ttys are not available).  This causes too much input to be
    // buffered.  (The problem seems to be in java.io.InputStreamReader.)

    int count;
    for (count = 0;  count < len;  )
      {
	int ch = in.read();
	if (ch < 0)
	  {
	    if (count == 0)
	      return -1;
	    break;
	  }
	buffer[off++] = (char) (0xff & ch);
	count++;
	if (ch == '\n' || ch == '\r')
	  break;
      }
    if (tie != null)
      tie.echo(buffer, off, len);
    return count;
  }
}

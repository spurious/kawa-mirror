package kawa.lang;
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
    /* This explanation and work-around comes frm jacl 1.0's Shell.java:
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
    return super.fill(buffer, off, len);
  }
}

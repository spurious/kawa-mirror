package gnu.jemacs.buffer;
import java.io.*;
import gnu.mapping.*;
import javax.swing.text.*;

/** Insert input from an InputStream into a buffer.
 * This is typically output from an inferior process. */

class InputStreamHandler extends Thread
{
  ProcessMode mode;
  InputStream in;
  BufferWriter wr;
  Reader in_r;
  char[] buffer;

  public InputStreamHandler(InputStream in, ProcessMode mode)
  {
    this.in = in;
    this.in_r = new InputStreamReader(in);
    this.wr = new BufferWriter(mode.processMark, true);
    this.mode = mode;
  }

  public void run()
  {
    try
      {
	buffer = new char[512];
	System.err.println("read from inferior");
	for (;;)
	  {
	    int avail = in_r.read(buffer);
	    System.err.println("got "+avail+": '"+new String(buffer, 0, avail)+"'");
	    if (avail <= 0)
	      break;
	    wr.buffer = buffer;
	    wr.count = avail;
	    javax.swing.SwingUtilities.invokeAndWait(wr);
	    //wr.write(buffer, 0, avail);
	    //wr.flush();
	  }
	in.close();
      }
    catch (Exception ex)
      {
	throw new WrappedException(ex);
      }
  }
}

package kawa;
import gnu.expr.*;
import gnu.mapping.*;

public class TelnetRepl extends Procedure0
{
  // close when finished.
  java.net.Socket socket;

  Interpreter interp;

  public TelnetRepl(Interpreter interp, java.net.Socket socket)
  {
    this.interp = interp;
    this.socket = socket;
  }

  public Object apply0 ()
  {
    try
      {
	Shell.run(interp, Environment.getCurrent());
	return Values.empty;
      }
    finally
      {
	try
	  {
	    socket.close();
	  }
	catch (java.io.IOException ex)
	  {
	  }
      }
  }


  /** Run a Kawa repl as a telnet server.
      @param client A client that has connected to us,
      and that wants to use the telnet protocol to talk to a
      Scheme read-eval-print-loop. */
  public static void serve (Interpreter interp, java.net.Socket client)
    throws java.io.IOException
  {
    Telnet conn = new Telnet(client, true);
    java.io.OutputStream sout = conn.getOutputStream();
    java.io.InputStream sin = conn.getInputStream();
    OutPort out = new OutPort(sout);
    TtyInPort in = new TtyInPort(sin, "<stdin>", out);
    /*
    conn.request(Telnet.DO, Telnet.EOF);
    conn.request(Telnet.DO, Telnet.NAWS);
    conn.request(Telnet.DO, Telnet.TTYPE);
    conn.request(Telnet.DO, Telnet.LINEMODE);
    */

    Thread thread = new Future(new TelnetRepl(interp, client),
			       interp.getEnvironment(),
			       in, out, out);
    thread.start();
  }
}


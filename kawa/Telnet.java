package kawa;  // For now

/** A minimal telnet client. */


// Some ideas from:  Flanagan:  "Java Examples in a Nutshell" (O'Reilly, 1997),
// example 9-8.

public class Telnet extends Thread
{
  TelnetConnection connection;

  static void usage ()
  {
    System.err.println("Usage:  [java] kawa.Telnet HOST [PORT#]");
    System.exit(-1);
  }

  public static void main (String[] args)
  {
    if (args.length == 0)
      usage();
    String host = args[0];
    int port = 23;
    if (args.length > 1)
      {
	port = Integer.parseInt(args[1]);
      }
    try
      {
	java.net.Socket socket = new java.net.Socket(host, port);
	TelnetConnection connection = new TelnetConnection(socket, false);
	TelnetOutputStream tout = connection.getOutputStream();
	Telnet t = new Telnet (connection);

	t.setPriority(Thread.currentThread().getPriority() + 1);
	t.start();

	byte[] buffer = new byte[1024];
	for (;;)
	  {
	    int ch = System.in.read();
	    if (ch < 0)
	      break; // send EOF FIXME
	    buffer[0] = (byte) ch;
	    int avail = System.in.available();
	    if (avail > 0)
	      {
		if (avail > buffer.length-1)
		  avail = buffer.length - 1;
		avail = System.in.read(buffer, 1, avail);
	      }
	    tout.write(buffer, 0, avail+1);
	  }
	t.stop();
      }
    catch (Exception ex)
      {
	System.err.println(ex);
      }
  }

  private Telnet (TelnetConnection connection)
  {
    this.connection = connection;
  }

  public void run ()
  {
    try
      {
	TelnetInputStream tin = connection.getInputStream();
	byte[] buffer = new byte[1024];
	for (;;)
	  {
	    int ch = tin.read();
	    if (ch < 0)
	      break; // ??? FIXME
	    buffer[0] = (byte) ch;
	    int avail = tin.available();
	    if (avail > 0)
	      {
		if (avail > buffer.length-1)
		  avail = buffer.length - 1;
		avail = tin.read(buffer, 1, avail);
	      }
	    System.out.write(buffer, 0, avail+1);
	  }
      }
    catch (java.io.IOException ex)
      {
	System.err.println(ex);
	System.exit(-1);
      }
  }
}

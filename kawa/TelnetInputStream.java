package kawa;  // For now
import java.io.*;

/**
 * An input stream that handles the telnet protocol.
 * It handles the special telnet sequences starting with the
 * "Interpret As Command".(IAC==255) byte.
 */

public class TelnetInputStream extends FilterInputStream
{
  TelnetConnection connection;

  public TelnetInputStream (InputStream in, TelnetConnection conn)
    throws IOException
  {
    super(in);
    buf = new byte[512];
    this.connection = conn;
  }

  protected byte[] buf;

  int pos;
  int count;

  /** The state of control bytes we have seen. */
  int state = 0;

  int subCommandLength = 0;

  static final int SB_IAC = 400;

  public int read () throws IOException
  {
    for (;;)
      {
	if (pos >= count)
          {
	    int avail = in.available();
	    if (avail <= 0)
	      avail = 1;
	    else if (avail > buf.length - subCommandLength)
	      {
		avail = buf.length - subCommandLength;  // FIXME
	      }
	    avail = in.read(buf, subCommandLength, avail);
	    pos = subCommandLength;
	    count = avail;
	    if (avail <= 0)
	      return -1;
	  }
	int ch = buf[pos++] & 0xff;
	if (state == 0)
	  {
	    if (ch != TelnetConnection.IAC)
	      return ch;
	    state = TelnetConnection.IAC;
	    continue;
	  }
	else if (state == TelnetConnection.IAC)
	  {
	    if (ch == TelnetConnection.IAC)
	      {
		state = 0;
		return TelnetConnection.IAC;
	      }
	    else if (ch == TelnetConnection.WILL
		     || ch == TelnetConnection.WONT
		     || ch == TelnetConnection.DO
		     || ch == TelnetConnection.DONT
		     || ch == TelnetConnection.SB)
	      {
		state = ch;
	      }
	    else if (ch == TelnetConnection.IP)
	      {
		System.err.println("Interrupt Process");
		state = 0;
	      }
	    else if (ch == TelnetConnection.EOF)
	      {
		return -1;
	      }
	    else
	      {
		state = 0; // ???
	      }
	  }
	else if (state == TelnetConnection.WILL || state == TelnetConnection.WONT
		 || state == TelnetConnection.DO || state == TelnetConnection.DONT)
	  {
	    connection.handle (state, ch);
	    state = 0;
	  }
	else if (state == TelnetConnection.SB)
	  {
	    if (ch == TelnetConnection.IAC)
	      state = SB_IAC;
	    else
	      buf[subCommandLength++] = (byte) ch;
	  }
	else if (state == SB_IAC)
	  {
	    if (ch == TelnetConnection.IAC)
	      {
		buf[subCommandLength++] = (byte) ch;
		state = TelnetConnection.SB;
	      }
	    else if (ch == TelnetConnection.SE)
	      {
		connection.subCommand(buf, 0, subCommandLength);
		state = 0;
		subCommandLength = 0;
	      }
	    else // Error?
	      {
		state = 0;
		subCommandLength = 0;
	      }
	  }
	else
	  System.err.println("Bad state "+state);
      }
  }

  public int read (byte[] b, int offset, int length) throws IOException
  {
    if (length <= 0)
      return 0;
    int done = 0;
    if (state != 0 || pos >= count)
      {
	int ch = read();
	if (ch < 0)
	  return ch;
	b[offset++] = (byte) ch;
	done++;
      }
    if (state == 0)
      {
	while (pos < count && done < length)
	  {
	    byte ch = buf[pos];
	    if (ch == (byte) TelnetConnection.IAC)
	      break;
	    b[offset++] = ch;
	    done++;
	    pos++;
	  }
      }
    return done;
  }
}

package kawa.standard;
import gnu.text.Char;
import gnu.lists.*;
import java.io.Reader;
import java.io.InputStream;
import gnu.mapping.Procedure0or1;
import gnu.mapping.WrongType;
import gnu.mapping.InPort;

public class readchar extends Procedure0or1
{
  public static final readchar readChar = new readchar(false);
  public static final readchar peekChar = new readchar(true);

  boolean peeking;
  public readchar(boolean peeking)
  {
    super(peeking ? "peek-char" : "read-char");
    this.peeking = peeking;
  }

  final Object readChar (InPort port)
  {
    try
      {
	int ch = peeking ? port.peek() : port.read();
	if (ch < 0)
	  return Sequence.eofValue;
	return Char.make (ch);
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException ("IO Exception caught");
      }
  }

  final Object readChar (Reader port)
  {
    try
      {
	int ch;
	if (peeking)
	  {
	    port.mark(1);
	    ch = port.read();
	    port.reset();
	  }
	else
	  ch = port.read();
	if (ch < 0)
	  return Sequence.eofValue;
	return Char.make (ch);
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException ("IO Exception caught");
      }
  }

  final Object readChar (InputStream port)
  {
    try
      {
	int ch;
	if (peeking)
	  {
	    port.mark(1);
	    ch = port.read();
	    port.reset();
	  }
	else
	  ch = port.read();
	if (ch < 0)
	  return Sequence.eofValue;
	return Char.make (ch);
      }
    catch (java.io.IOException e)
      {
	throw new RuntimeException ("IO Exception caught");
      }
  }

  public final Object apply0 ()
  {
    return readChar (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
  {
    if (arg1 instanceof InPort)
      return readChar ((InPort) arg1);
    if (arg1 instanceof Reader)
      return readChar ((Reader) arg1);
    if (arg1 instanceof InputStream)
      return readChar ((InputStream) arg1);
    throw new WrongType (this, 1, arg1, "<input-port>");
  }
}

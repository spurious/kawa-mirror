package kawa.standard;
import kawa.lang.*;
import java.io.Reader;
import java.io.InputStream;

public class readchar extends Procedure0or1
{
  boolean peeking;
  public readchar(boolean peeking)
  {
    super(peeking ? "peek-char" : "read-char");
    this.peeking = peeking;
  }

  final Object readChar (InPort port)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
	throw new GenericError ("IO Exception caught");
      }
  }

  final Object readChar (Reader port)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
	throw new GenericError ("IO Exception caught");
      }
  }

  final Object readChar (InputStream port)
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
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
	throw new GenericError ("IO Exception caught");
      }
  }

  public final Object apply0 ()
      throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    return readChar (InPort.inDefault());
  }

  public final Object apply1 (Object arg1)
       throws WrongArguments, WrongType, GenericError, UnboundSymbol
  {
    if (arg1 instanceof InPort)
      return readChar ((InPort) arg1);
    if (arg1 instanceof Reader)
      return readChar ((Reader) arg1);
    if (arg1 instanceof InputStream)
      return readChar ((InputStream) arg1);
    throw new WrongType (this.name (), 1, "input port");
  }
}

package gnu.lists;
import java.io.*;

/** A Pair with the file name and position it was read from. */

public class PairWithPosition extends Pair
{
  String filename;
  /** An encoding of lineNumber+(columnNumber<<20).
   * Note if columnNumber is unspecified (0), then position is lineNumber. */
  int position;

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    position = lineno + (colno << 20);
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFile ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this pair.
    * The "first" line is line 1. */
  public final int getLine ()
  {
    return position & 0xFFFFF;
  }

  public final int getColumn ()
  {
    return position >>> 20;
  }

  /** Get the line number of (the start of) this pair.
    * The "first" line is line 1. */
  public PairWithPosition (gnu.text.LineBufferedReader port,
			   Object car, Object cdr)
  {
    super (car, cdr);
    filename = port.getName ();
    setLine (port.getLineNumber() + 1, port.getColumnNumber() + 1);
  }

  public PairWithPosition (PairWithPosition where,
                           Object car, Object cdr)
  {
    super (car, cdr);
    filename = where.filename;
    position = where.position;
  }

  public PairWithPosition (Object car, Object cdr)
  {
    super (car, cdr);
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, int line, int column)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.setLine(line, column);
    return pair;
  }

  public static PairWithPosition make(Object car, Object cdr,
				      String filename, int position)
  {
    PairWithPosition pair = new PairWithPosition(car, cdr);
    pair.filename = filename;
    pair.position = position;
    return pair;
  }

  /**
   * @serialData Write the car followed by the cdr,
   *   followed by filename (as an Object, so it can be shared),
   *   followed by position (line|(column<<20)).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(car);
    out.writeObject(cdr);
    out.writeObject(filename);
    out.writeInt(position);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    car = in.readObject();
    cdr = in.readObject();
    filename = (String) in.readObject();
    position = in.readInt();
  }
}

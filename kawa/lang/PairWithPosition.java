package kawa.lang;

/** A Pair with the file name and position it was read from. */

public class PairWithPosition extends Pair
{
  String filename;
  int position;

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
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
    return position >> 12;
  }

  public final int getColumn ()
  {
    return position & ((1 << 12) - 1);
  }

  public PairWithPosition (InPort port, Object car, Object cdr)
  {
    super (car, cdr);
    filename = port.getName ();
    setLine (port.getLineNumber (), port.getColumnNumber ());
  }

  public PairWithPosition (Object car, Object cdr)
  {
    super (car, cdr);
  }
}

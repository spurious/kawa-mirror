package kawa.lang;

/** A Pair with the file name and position it was read from. */

public class PairWithPosition extends Pair
{
  String filename;
  int position;

  final void setFile (String filename)
  {
    this.filename = filename;
  }

  final void setLine (int lineno, int colno)
  {
    position = (lineno << 12) + colno;
  }

  final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  final String getFile ()
  {
    return filename;
  }

  final int getLine ()
  {
    return position >> 12;
  }

  final int getColumn ()
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

package gnu.emacs;

public final class Marker
{
  Buffer buffer;
  javax.swing.text.Position position;

  public int getDot()
  {
    if (position == null)
      return -1;
    else
      return position.getOffset();
  }

  public Buffer getBuffer()
  {
    return buffer;
  }
}

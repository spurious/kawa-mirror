package gnu.kawa.util;

/** A position (between two characters) in an abstract string.
 * Similar to javax.swing.text.Position.
 */

public class Position
{
  AbstractString data;

  int position;

  public Position()
  {
  }

  public Position(AbstractString data, int offset, int kind)
  {
    this.data = data;
    this.position = data.createPosition(offset, kind);
  }

  public int getOffset()
  {
    return data.getPositionOffset(position);
  }

  public void finalize()
  {
    data.releasePosition(position);
  }
}

package gnu.kawa.util;

public class SubString extends AbstractString
{
  AbstractString base;
  int startPosition;
  int endPosition;

  public SubString(AbstractString base, int startPosition, int endPosition)
  {
    this.base = base;
    this.startPosition = startPosition;
    this.endPosition = endPosition;
  }

  public final int length()
  {
    return base.getPositionOffset(endPosition)
      - base.getPositionOffset(startPosition);
  }

  public final char charAt(int index)
  {
    int end = base.getPositionOffset(endPosition);
    int start = base.getPositionOffset(startPosition);
    if (index < 0 || index >= end - start)
      throw new StringIndexOutOfBoundsException(index);
    return base.charAt(index + start);
  }

  public final void setCharAt(int index, char ch)
  {
    int end = base.getPositionOffset(endPosition);
    int start = base.getPositionOffset(startPosition);
    if (index < 0 || index >= end - start)
      throw new StringIndexOutOfBoundsException(index);
    base.setCharAt(index + start, ch);
  }

  public void getChars (int srcBegin, int srcEnd, char[] dst, int dstBegin)
  {
    int start = base.getPositionOffset(startPosition);
    int end = base.getPositionOffset(endPosition);
    srcBegin += start;
    srcEnd += start;
    if (srcEnd > end)
      throw new StringIndexOutOfBoundsException();
    base.getChars(srcBegin, srcEnd, dst, dstBegin);
  }

  public String substring(int startIndex, int endIndex)
  {
    int start = base.getPositionOffset(startPosition);
    int end = base.getPositionOffset(endPosition);
    startIndex += start;
    endIndex += start;
    if (startIndex > endIndex || endIndex > end)
      throw new StringIndexOutOfBoundsException(endIndex);
    return base.substring(startIndex, endIndex);
  }

  /** Make a shared sub-string, using position values.
   */
  public AbstractString subString(int fromPosition, int toPosition)
  {
    return base.subString(fromPosition, toPosition);
  }

  public void writeTo (int start, int count, java.io.Writer dest)
    throws java.io.IOException
  {
    base.writeTo(start, count + base.getPositionOffset(startPosition), dest);
  }

  /** Insert count unspecified (garbage) characters at where.
   * Moves the gapStart after the inserted characters. */
  protected void insert(int where, int count, boolean beforeMarkers)
  {
    int start = base.getPositionOffset(startPosition);
    base.insert(where - start, count, beforeMarkers);
  }

  public int createPosition (int offset, int kind)
  {
    int start = base.getPositionOffset(startPosition);
    return base.createPosition(offset + start, kind);
  }

  public int getStartPosition()
  {
    return startPosition;
  }

  public int getEndPosition()
  {
    return endPosition;
  }

  public void releasePosition (int position)
  {
    if (position != startPosition && position != endPosition)
      base.releasePosition(position);
  }

  public int getPositionOffset (int position)
  {
    return base.getPositionOffset(position);
  }

  public int getPositionKind (int position)
  {
    return base.getPositionKind(position);
  }

  public void finalize()
  {
    base.releasePosition(startPosition);
    base.releasePosition(endPosition);
  }
}

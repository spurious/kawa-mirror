package gnu.jemacs.buffer;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;

/** A Content class that supportd Emacs-style Markers.
 * The standard GapContent is close, but unfortunately it only
 * supports inserting *before* marks, whihc is not the Emacs default.
 * This provides a superset of the Position functionality (except for undo).
 * (Long-term, it would be more efficient to provide a complete replacement
 * for GapContent that does not use GapContent.)
 */

public class BufferContent extends GapContent
{
  int firstPosition[] = new int[4];

  /** Number of marks that stay before inserted text. */
  private int numBeforePositions() { return firstPosition[1]; }
  /** Number of marks that stay after text inserted with
   * insert-before-markers, and before text inserted with afterMarkers. */
  private int numMediumPositions() { return firstPosition[2] - firstPosition[1]; }
  /** Number of marks that stay after inserted text. */
  private int numAfterPositions() { return firstPosition[3] - firstPosition[2]; }
  private int numPositions() { return firstPosition[3]; }

  /** An array of buffer positions.
   * First come numBeforePositions, then numMediumPositions, then numAfterPositions.
   * Each segment is sorted by increasing offset.
   * For now these are logical buffer indexes;  in the future, they should
   * be physical indexes, counting a possible gap. */
  int[] positions;

  /** Each Marker (and in the future each Position) has an index into
   * this table, which specified an index in positions. */
  int[] indexes;

  /** Allocate an index in the positions array. */
  int allocatePositionIndex(int offset, int kind)
  {
    int start = firstPosition[kind] - firstPosition[kind-1];
    int end = firstPosition[kind+1] - firstPosition[kind];
    while (start < end)
      {
        int mid = (start + end) >> 1;
        int midval = positions[mid];
        if (midval == offset)
          return mid;
        if (offset < midval)
          end = mid;
        else
          start = mid + 1;
      }
    if (positions == null)
      positions = new int[8];
    else if (numPositions() == positions.length)
      {
        int[] tmp = new int[2 * positions.length];
        System.arraycopy(positions, 0, tmp, 0, positions.length);
        positions = tmp;
      }
    System.arraycopy(positions, start, positions, start+1,
                     numPositions() - start);
    positions[start] = offset;
    for (int i = kind;  ++i < 4;  )
      firstPosition[i]++;
    return start;
  }

  public int allocatePosition(int offset, int kind)
  {
    int position = allocatePositionIndex(offset, kind);
    return allocateFromPosition(position);
  }
  public int allocateFromPosition(int position)
  {
    if (indexes == null)
      {
        indexes = new int[10];
        for (int j = 10;  --j >= 0; )
          indexes[j] = -1;
      }
    for (int i = 0;  ;  i++)
      {
        if (i == indexes.length)
          {
            int[] tmp = new int[2 * indexes.length];
            System.arraycopy(indexes, 0, tmp, 0, indexes.length);
            for (int j = tmp.length;  --j >= indexes.length; )
              tmp[j] = -1;
            indexes = tmp;
          }
        if (indexes[i] < 0)
          {
            indexes[i] = position;
            return i;
          }
      }
  }

  public void freePositionIndex(int offset)
  {
    System.arraycopy(positions, offset+1, positions, offset,
                     numPositions() - 1);
    for (int i = indexes.length; --i >= 0; )
      {
        if (indexes[i] > offset)
          indexes[i]--;
      }
    for (int i = 4;  --i > 0; )
      {
        int position = firstPosition[i];
        if (position > offset)
          firstPosition[i] = position - 1;
      }
  }

  public void freePosition(int id)
  {
    int index = indexes[id];
    indexes[id] = -1;
    for (int i = indexes.length; --i >= 0; )
      {
        if (indexes[i] == index)
          return; // Still in use.
      }
    freePositionIndex(index);
  }

  public UndoableEdit remove(int where, int nitems)
    throws BadLocationException
  {
    for (int j = numPositions();  --j >= 0; )
      {
        int position = positions[j];
        if (position > where)
          {
            if (position > where + nitems)
              position -= nitems;
            else
              position = where;
            positions[j] = position;
          }
      }
    return super.remove(where, nitems);
  }

  public UndoableEdit
  insertString(int where, String str, boolean beforeMarkers)
    throws BadLocationException
  {
    int size = str.length();
    int j = 0;
    int limit = firstPosition[beforeMarkers ? 1 : 2];
    for (; j < limit;  j++)
      {
        int position = positions[j];
        if (position > where)
          positions[j] = position + size;
      }
    for (; j < firstPosition[3];  j++)
      {
        int position = positions[j];
        if (position >= where)
          positions[j] = position + size;
      }
    return super.insertString(where, str);
  }

  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    return insertString(where, str, false);
  }
}

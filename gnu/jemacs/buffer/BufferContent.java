package gnu.jemacs.buffer;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;

/** A Content class that supports Emacs-style Markers.
 * The standard GapContent is close, but unfortunately it only
 * supports inserting *before* marks, which is not the Emacs default.
 * This provides a superset of the Position functionality (except for undo).
 */

public class BufferContent
implements javax.swing.text.AbstractDocument.Content
{
  // There are 4 == (number kinds of marker) + 1.
  int firstPosition[] = new int[4];

  /** Kind of mark which always stays before text inserted at the mark.
   * An example is the pseudo-mark representing the beginning of the buffer. */
  public static final int BEFORE_MARK_KIND = 0;

  /** Kind of mark which stays before or after text inserted at the mark,
   * depending on whether plain insert or insert-before-markers is called.
   * This is how standard Emacs markers behave. */
  public static final int EMACS_MARK_KIND = 1;

  /** Kind of mark which always stays after text inserted at the mark.
   * An example is the standard behavior of javax.swing.text.Position;
   * another is the pseudo-mark representing the end of the buffer;
   * another is the pseudo-mark representing the Emacs insertion point. */
  public static final int AFTER_MARK_KIND = 2;

  protected char[] array;
  protected int gapStart;
  protected int gapEnd;

  /** Number of marks whose kind is BEFORE_MARK_KIND. */
  private int numBeforePositions() { return firstPosition[1]; }

  /** Number of marks whose kind is EMACS_MARK_KIND. */
  private int numMediumPositions() { return firstPosition[2] - firstPosition[1]; }

  /** Number of marks whose kind is AFTER_MARK_KIND. */
  private int numAfterPositions() { return firstPosition[3] - firstPosition[2]; }
  private int numPositions() { return firstPosition[3]; }

  /** An array of buffer positions.
   * First come numBeforePositions, then numMediumPositions, then numAfterPositions.
   * Each segment is sorted by increasing offset.
   * For now these are logical buffer indexes;  in the future, they should
   * perhaps be physical indexes, counting a possible gap. */
  int[] positions;

  /** Each Marker (and in the future each Position) has an index into
   * this table, which specifies an index in positions. */
  int[] indexes;

  public BufferContent()
  {
    this(100);
  }

  public BufferContent(int initialSize)
  {
    array = new char[initialSize];
    // Swing seems to assume that a Content object is initialized
    // containing a single '\n'.  This of course is not documented ...
    // (A cleaner solution might be to initialize this as empty, but have
    // Buffer insetr the initial '\n'.  FIXME.)
    gapEnd = initialSize-1;
    array[gapEnd] = '\n';
    gapStart = 0;
  }

  /** Allocate an index in the positions array. */
  int allocatePositionIndex(int offset, int kind)
  {
    int start = firstPosition[kind];
    int end = firstPosition[kind+1];
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
    for (int i = indexes == null ? 0 : indexes.length; --i >= 0; )
      {
        int index = indexes[i];
        if (index >= start)
          indexes[i] = index + 1;
      }
    return start;
  }

  /** Given a buffer offset, allocate in index in indexes. */
  int allocatePosition(int offset, int kind)
  {
    int position = allocatePositionIndex(offset, kind);
    return allocateFromPosition(position);
  }

  /** Find an available slot in indexes.
   * @param position an index in the positions array.
   * @return a previously-free slot in indexes, now set to position.
   */
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

  protected void shiftGap(int newGapStart)
  {
    int delta = newGapStart - gapStart;
    if (delta > 0)
      {
        System.arraycopy(array, gapEnd, array, gapStart, delta);
      }
    else if (delta < 0)
      {
        System.arraycopy(array, newGapStart, array, gapEnd + delta, - delta);
      }
    gapEnd += delta;
    gapStart = newGapStart;
  }

  public final int length()
  {
    return array.length - (gapEnd - gapStart);
  }

  public final char charAt(int index)
  {
     if (index >= gapStart)
       index += gapEnd - gapStart;
     return array[index];
  }

  protected int getChars(int where, int len)
    throws BadLocationException
  {
    int length = length();
    if (where < 0 || where > length)
      throw new BadLocationException("bad location (in getChars)", where);
    if (len < 0)
      len = 0;
    else if (where + len > length)
      len = length - where;
    // if (len < 0 || where + len > length)
    //   throw new BadLocationException("bad length", len);
    if (where + len <= gapStart)
      return where;
    else if (where < gapStart)
      shiftGap(where);
    return where + (gapEnd - gapStart);
  }

  public void getChars(int where, int len, Segment txt)
    throws BadLocationException
  {
    txt.offset = getChars(where, len);
    txt.array = array;
    txt.count = len;
  }

  public String getString(int where, int len)
    throws BadLocationException
  {
    return new String(array, getChars(where, len), len);
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

    if (where != gapStart)
      {
        if (where < 0 || where >= length())
          throw new BadLocationException("bad location for remove", where);
        shiftGap(where);
      }
    int endSize = array.length - gapEnd;
    if (nitems < 0 || nitems > endSize)
      throw new BadLocationException("bad coun for remove", nitems);
    gapEnd += nitems;

    GapUndoableEdit undo = new GapUndoableEdit();
    undo.content = this;
    undo.data = new String(array, gapEnd - nitems, nitems);
    undo.nitems = nitems;
    undo.isInsertion = false;
    undo.startIndex = allocatePosition(where, BEFORE_MARK_KIND);
    return undo;
  }

  /** Adjust gap to where, and make sure it is least `size' chars long. */
  public void gapReserve(int where, int size)
    throws BadLocationException
  {
    int oldlen = length();
    if (where < 0 || where > oldlen)
      throw new BadLocationException("bad location (in gapReserve)", where);
    if (oldlen + size > array.length)
      {
        int newlen;
        if (oldlen < 20000)
          newlen = 2 * oldlen;
        else
          newlen = oldlen + 4000;
        int minlen = oldlen + size + 100;
        if (minlen < newlen)
          newlen = minlen + 500;
        char[] newarray = new char[newlen];
        if (where <= gapStart)
          {
            System.arraycopy(array, 0, newarray, 0, where);
            int moveSize = gapStart - where;
            int endSize = oldlen - gapEnd;
            System.arraycopy(array, where,
                             newarray, newlen - endSize - moveSize, moveSize);
            System.arraycopy(array, gapEnd, newarray, newlen-endSize, endSize);
            gapEnd = newlen - endSize - moveSize;
          }
        else
          {
            System.arraycopy(array, 0, newarray, 0, gapStart);
            System.arraycopy(array, gapEnd, newarray, gapStart,
                             where - gapStart);
            int whereOffset = where + (gapEnd - gapStart);
            int endSize = oldlen - whereOffset;
            gapEnd = newlen - endSize;
            System.arraycopy(array, whereOffset, newarray, gapEnd, endSize);
          }
        gapStart = where;
      }
    else if (where != gapStart)
      {
        shiftGap(where);
      }
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

    int slen = str.length();
    gapReserve(where, slen);
    str.getChars(0, slen, array, where);
    gapStart += slen;

    GapUndoableEdit undo = new GapUndoableEdit();
    undo.content = this;
    undo.data = str;
    undo.nitems = slen;
    undo.isInsertion = true;
    undo.startIndex = allocatePosition(where, BEFORE_MARK_KIND);
    return undo;
  }

  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    return insertString(where, str, false);
  }

  public Position createPosition(int offset)
    throws BadLocationException
  {
    // A weird hack, but this seems to be what Swing does ...
    int kind = offset == 0 ? BEFORE_MARK_KIND : AFTER_MARK_KIND;

    if (offset < 0 || offset > length())
      throw new BadLocationException("bad offset to createPosition", offset);
    GapPosition pos = new GapPosition();
    pos.content = this;
    pos.index = allocatePosition(offset, kind);
    return pos;
  }

  public void dump()
  {
    System.err.print("before gap: \"");
    System.err.print(new String(array, 0, gapStart));
    System.err.println("\" (gapStart:"+gapStart+" gapEnd:"+gapEnd+')');
    System.err.print("after gap: \"");
    System.err.print(new String(array, gapEnd, array.length-gapEnd));
    System.err.println("\"");
    for (int kind = 0;  kind < 3;  kind++)
      {
        int start = firstPosition[kind];
        int end = firstPosition[kind+1];
        String[] kinds = { "before", "emacs", "after" };
        System.err.println("Positions (kind: "+kinds[kind]+"):");
        for (int i = start;  i < end;  i++)
          {
            System.err.println("position#"+i+": "+positions[i]);
          }
      }
    if (indexes != null)
      {
        System.err.println("marker indexes:");
        for (int i = 0;  i < indexes.length;  i++)
          {
            if (indexes[i] >= 0)
              System.err.println("index#"+i+": "+indexes[i]
                                 +" (offset:"+positions[indexes[i]]+")");
          }
      }
  }

  public static int indexOf(char[] buffer, int start, int limit, char ch)
  {
    for (int i = start; i < limit; i++)
      {
        if (buffer[i] == ch)
          return i;
      }
    return -1;
  }

  /** Search for the last occurrence of a character
   * in buffer[limit..start]. */
  public static int lastIndexOf(char[] buffer, int start, int limit, char ch)
  {
    for (int i = start; i >= limit; i--)
      {
        if (buffer[i] == ch)
          return i;
      }
    return -1;
  }

  /** Search in BUF for COUNT instances of the character TARGET between START and END.
   * If COUNT is positive, search forwards; END must be >= START.
   * If COUNT is negative, search backwards for the -COUNTth instance;
   *   END must be <= START.
   * If COUNT is zero, do anything you please; run rogue, for all I care.
   *
   * If we find COUNT instances, SHORTAGE is zero, and return the
   * position after the COUNTth match.  Note that for reverse motion
   * this is not the same as the usual convention for Emacs motion commands.

   * If we don't find COUNT instances before reaching END, set SHORTAGE
   * to the number of TARGETs left unfound, and return (shortage<<32|END).
   * @return (SHORTAGE<<32|POS)
  */

  public final long scan(char target, int start, int end,
                         int count, boolean allowQuit)
  {
    int limit = end > gapStart ? end + gapEnd - gapStart : end;
    if (start > gapStart)
      start += gapEnd - gapStart;
    if (count > 0)
      {
        while (start < limit && count > 0)
          {
            int ceil;
            if (start == gapStart)
              start = gapEnd;
            if (start < gapStart && limit > gapStart)
              ceil = gapStart;
            else
              {
                ceil = limit;
              }
            if (allowQuit)
              {
                if (ceil - start > 5000)
                  ceil = start + 5000;
                Emacs.checkQuit();
              }
            int i = indexOf(array, start, ceil, target);
            if (i >= 0)
              {
                count--;
                start = i + 1;
              }
            else
              start = ceil;
          }
        if (start > gapEnd)
          start -= gapEnd - gapStart;
        return (count << 32) | start;
      }
    else
      {
        while (start > limit && count < 0)
          {
            if (start == gapEnd)
              start = gapStart;
            int floor;
            if (start <= gapStart || limit >= gapEnd)
              floor = limit;
            else
              floor = gapEnd;
            if (allowQuit)
              {
                if (start - floor > 5000)
                  floor = start - 5000;
                Emacs.checkQuit();
              }
            int i = lastIndexOf(array, start - 1, floor, target);
            if (i >= 0)
              {
                count++;
                start = i;
              }
            else
              start = floor;
          }
  
        if (start > gapEnd)
          start -= gapEnd - gapStart;
        if (count != 0)
          return ((long) (- count) << 32) | start;
        else
          {
            // We found the character we were looking for; we have to return
            // the position *after* it due to the strange way that the return
            // value is defined.
            return start + 1;
          }
      }
  }
}

class GapPosition implements Position
{
  BufferContent content;

  /** The index of the current position in content.map. */
  int index;

  public int getOffset()
  {
    return content.positions[content.indexes[index]];
  }

  public void finalize()
  {
    content.freePosition(index);
  }

}

class GapUndoableEdit extends AbstractUndoableEdit
{
  // False if this is a remove (delete);  true if an insertion.
  boolean isInsertion;

  BufferContent content;

  String data;

  int startIndex;
  int nitems;

  public void finalize()
  {
    content.freePosition(startIndex);
  }

  public void die()
  {
    super.die();
    content.freePosition(startIndex);
    startIndex = -1;
  }

  private void doit(boolean isInsertion)
    throws BadLocationException
  {
    int startOffset = content.positions[content.indexes[startIndex]];
    if (isInsertion)
      {
        // FIXME returns useless Undo
        content.insertString(startOffset, data);
      }
    else
      {
        // FIXME returns useless Undo
        content.remove(startOffset, nitems);
      }
  }

  public void undo () throws CannotUndoException
  {
    super.undo();
    try
      {
        doit (! isInsertion);
      }
    catch (BadLocationException ex)
      {
        throw new CannotUndoException();
      }
  }

  public void redo () throws CannotUndoException
  {
    super.redo();
    try
      {
        doit (isInsertion);
      }
    catch (BadLocationException ex)
      {
        throw new CannotRedoException();
      }
  }
}

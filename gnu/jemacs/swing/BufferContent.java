package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import gnu.lists.*;

/** A Content class that supports Emacs-style Markers.
 * The standard GapContent is close, but unfortunately it only
 * supports inserting *before* marks, which is not the Emacs default.
 * This provides a superset of the Position functionality (except for undo).
 */

public class BufferContent extends CharBuffer
implements javax.swing.text.AbstractDocument.Content
{
  public BufferContent()
  {
    this(100);
  }

  public BufferContent(int initialSize)
  {
    super(initialSize);
    // Swing seems to assume that a Content object is initialized
    // containing a single '\n'.  This of course is not documented ...
    // (A cleaner solution might be to initialize this as empty, but have
    // Buffer insert the initial '\n'.  FIXME.)
    gapEnd = initialSize-1;
    getArray()[gapEnd] = '\n';
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
    if (where >= gapStart)
      return where + (gapEnd - gapStart);
    // Shift the gap depending in which direction needs least copying.
    if (gapStart - where > (len >> 1))
      {
	shiftGap(where + len);
	return where;
      }
    else
      {
	shiftGap(where);
	return where + (gapEnd - gapStart);
      }
  }

  public void getChars(int where, int len, Segment txt)
    throws BadLocationException
  {
    txt.offset = getChars(where, len);
    txt.array = getArray();
    txt.count = len;
  }

  public String getString(int where, int len)
    throws BadLocationException
  {
    return new String(getArray(), getChars(where, len), len);
  }

  public UndoableEdit remove(int where, int nitems)
    throws BadLocationException
  {
    if (nitems < 0 || where < 0 || where + nitems > length())
      throw new BadLocationException("invalid remove", where);

    delete(where, nitems);

    GapUndoableEdit undo = new GapUndoableEdit(where);
    undo.content = this;
    undo.data = new String(getArray(), gapEnd - nitems, nitems);
    undo.nitems = nitems;
    undo.isInsertion = false;
    return undo;
  }

  public UndoableEdit
  insertString(int where, String str, boolean beforeMarkers)
    throws BadLocationException
  {
    if (where < 0 || where > length())
      throw new BadLocationException("bad insert", where);
    insert(where, str, beforeMarkers);

    GapUndoableEdit undo = new GapUndoableEdit(where);
    undo.content = this;
    undo.data = str;
    undo.nitems = str.length();
    undo.isInsertion = true;
    return undo;
  }

  public UndoableEdit insertString(int where, String str)
    throws BadLocationException
  {
    return insertString(where, str, false);
  }

  public javax.swing.text.Position createPosition(int offset)
    throws BadLocationException
  {
    // A weird hack, but this seems to be what Swing does ...
    boolean isAfter = offset != 0;

    if (offset < 0 || offset > length())
      throw new BadLocationException("bad offset to createPosition", offset);
    return new GapPosition(this, offset, isAfter);
  }

  public void dump()
  {
    System.err.println("Buffer Content dump.  size:"+size()+"  buffer:"+getArray().length);
    System.err.print("before gap: \"");
    System.err.print(new String(getArray(), 0, gapStart));
    System.err.println("\" (gapStart:"+gapStart+" gapEnd:"+gapEnd+')');
    System.err.print("after gap: \"");
    System.err.print(new String(getArray(), gapEnd, getArray().length-gapEnd));
    System.err.println("\"");
    int poslen = positions == null ? 0 : positions.length;
    System.err.println("Positions (size: "+poslen+" free:"+free+"):");
    for (int i = 0;  i < poslen;  i++)
      {
	int pos = positions[i];
	if (free == -2 ? pos != FREE_POSITION : pos != 0)
	  System.err.println("position#"+i+": "+(pos>>1)+" isAfter:"+(pos&1));
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
                Signal.checkQuit();
              }
            int i = indexOf(getArray(), start, ceil, target);
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
        return ((long) count << 32) | start;
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
                Signal.checkQuit();
              }
            int i = lastIndexOf(getArray(), start - 1, floor, target);
            if (i >= 0)
              {
                count++;
                start = i;
              }
            else
              start = floor;
          }
  
        if (start >= gapEnd)
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

class GapPosition extends SeqPosition
    implements javax.swing.text.Position
{
  public GapPosition(CharBuffer content, int offset, boolean isAfter)
  {
    super(content, offset, isAfter);
  }

  public int getOffset() { return nextIndex(); }
}

class GapUndoableEdit extends AbstractUndoableEdit
{
  // False if this is a remove (delete);  true if an insertion.
  boolean isInsertion;

  BufferContent content;

  String data;

  int startOffset;
  int nitems;

  GapUndoableEdit(int offset)
  {
    startOffset = offset;
  }

  private void doit(boolean isInsertion)
    throws BadLocationException
  {
    //int startOffset = content.positions[content.indexes[startIndex]];
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

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

public class BufferContent extends gnu.kawa.swingviews.SwingContent
{
  public BufferContent()
  {
    this(100);
  }

  public BufferContent(int initialSize)
  {
    super(initialSize);
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
     * If COUNT is positive, search forwards; requires {@code END >= START}.
     * If COUNT is negative, search backwards for the -COUNTth instance;
     *   requires {@code END <= START}.
     * If COUNT is zero, do anything you please; run rogue, for all I care.
     * START and END are both 0-origin.
   *
   * If we find COUNT instances, SHORTAGE is zero, and return the
   * (0-origin) position after the COUNTth match.  Note that for reverse motion
   * this is not the same as the usual convention for Emacs motion commands.

   * If we don't find COUNT instances before reaching END, set SHORTAGE
   * to the number of TARGETs left unfound, and return {@code (shortage<<32|END)}.
   * @return {@code(SHORTAGE<<32|POS)}
   */
    public final long scan(char target, int start, int end,
                           int count, boolean allowQuit) {
        CharBuffer b = buffer;
        if (count > 0) {
            while (start < end && count > 0) {
                char[] data = b.getBuffer();
                long result = b.getSegment(start);
                int where = (int) result;
                int size = (int) (result >> 32);
                if (size > (end-start))
                    size = end-start;
                if (allowQuit) {
                    if (size > 10000)
                        size = 10000;
                    Signal.checkQuit();
                }
                int i = indexOf(data, where, where+size, target);
                if (i >= 0) {
                    count--;
                    start += i + 1;
                } else
                    start += size;
            }
            return ((long) count << 32) | start;
        } else {
            int sinceCheck = 0;
            while (start > end && count < 0) {
                char ch = b.charAt(--start);
                if (ch == target) {
                    count++;
                }
                if (allowQuit && ++sinceCheck > 10000) {
                    Signal.checkQuit();
                    sinceCheck = 0;
                }
            }
  
            if (count != 0)
                return ((long) (- count) << 32) | end;
            else {
                // We found the character we were looking for; we have to
                // return the position *after* it due to the strange way
                // that the return value is defined.
                return start + 1;
            }
        }
    }
}

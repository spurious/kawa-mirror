package gnu.jemacs.buffer;
import javax.swing.text.*;

public final class Marker implements Position
{
  Buffer buffer;

  /** The index of the current position in buffer.content.map. */
  int index;
  static final int POINT_POSITION_INDEX = -1;
  //static final int RESERVED_POSITION_COUNT = 1;

  public Marker()
  {
  }

  static final int EMACS_MARK_KIND = BufferContent.EMACS_MARK_KIND;

  public Marker(Marker marker)
  {
    buffer = marker.buffer;
    if (buffer != null)
      {
        BufferContent content = buffer.content;
        if (marker.index >= 0)
          index = content.allocateFromPosition(content.indexes[marker.index]);
        else
          index = content.allocatePosition(marker.getOffset(),EMACS_MARK_KIND);
      }
  }

  public void finalize()
  {
    if (buffer != null)
      buffer.content.freePosition(index);
  }

  public int getOffset()
  {
    if (buffer == null)
      return -1;
    else if (index == POINT_POSITION_INDEX)
      return buffer.curPosition.getDot();
    return buffer.content.positions[buffer.content.indexes[index]];
  }

  public int getPoint()
  {
    return 1 + getOffset();
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  public void setDot(int newPosition)
  {
    set(buffer, newPosition);
  }

  public void set(Buffer newBuffer, int newPosition)
  {
    if (this.index == POINT_POSITION_INDEX)
      {
        if (newBuffer != buffer)
          {
            String msg;
            if (newBuffer == null)
              msg = "Can't make point-marker point nowhere: ";
            else
              msg = "Can't change buffer of point-marker: ";
            throw new Error(msg+this);
          }
	buffer.curPosition.setDot(newPosition);
      }
    else
      {
        if (buffer != null)
          buffer.content.freePosition(index);
        if (newBuffer == null)
          {
            buffer = null;
            return;
          }

        if (newPosition < 0)
          newPosition = 0;
        else
          {
            int newLength = newBuffer.content.length();
            if (newPosition > newLength)
              newPosition = newLength;
          }
        int newIndex;
        if (buffer == newBuffer)
          newIndex = buffer.content.allocatePositionIndex(newPosition,
                                                          EMACS_MARK_KIND);
        else
          {
            buffer = newBuffer;
            newIndex = buffer.content.allocatePosition(newPosition,
                                                       EMACS_MARK_KIND);
          }
        buffer.content.indexes[index] = newIndex;
      }
  }

  public void deleteChar(int count)
  {
    int point = getOffset();
    try
      {
        if (count < 0)
          {
            point += count;
            count = - count;
          }
        buffer.document.remove(point, count);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
  }

  public void insert (String string, Style style)
  {
    int point = getOffset();
    try
      {
        buffer.document.insertString(point, string, style);
      }
    catch (javax.swing.text.BadLocationException ex)
      {
        throw new Error("bad location: "+ex);
      }
    point += string.length();
    setDot(point);
  }

  /** Insert count copies of ch at the current position. */
  public void insert (char ch, int count, Style style)
  {
    if (count < 0)
      return;
    int todo = count > 500 ? 500 : count;
    StringBuffer sbuf = new StringBuffer(todo);
    for (int i = todo;  --i >= 0; )
      sbuf.append(ch);
    String str = sbuf.toString();
    int point = getOffset();
    for (;;)
      {
	try
	  {
	    buffer.document.insertString(point, str, style);
	  }
	catch (javax.swing.text.BadLocationException ex)
	  {
	    throw new Error("bad location: "+ex);
	  }
	point += todo;
	count -= todo;
	if (count == 0)
	  break;
	if (count < 500)
	  {
	    todo = count;
	    sbuf.setLength(todo);
	    str = sbuf.toString();
	  }
      }
    setDot(point);
  }

  public int hashCode()
  {
    if (buffer == null)
      return 0;
    return buffer.hashCode() ^ getOffset();
  }

  public boolean equals (Object other)
  {
    if (! (other instanceof Marker))
      return false;
    Marker m2 = (Marker) other;
    return buffer == m2.buffer && getOffset() == m2.getOffset();
  }

  public String toString()
  {
    if (buffer == null)
      return "#<marker in no buffer>";
    StringBuffer sbuf = new StringBuffer(80);
    sbuf.append("#<marker at ");
    sbuf.append(getPoint());
    sbuf.append(" in ");
    sbuf.append(buffer.getName());
    sbuf.append('>');
    return sbuf.toString();
  }
}

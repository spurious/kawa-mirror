package gnu.jemacs.buffer;
import javax.swing.text.*;

/** A Writer that writes at a Buffer's poit or a Marker. */

public class BufferWriter extends java.io. Writer
{
  Marker marker;
  Style style;
  boolean adjustPoint;

  public BufferWriter (Marker marker, boolean adjustPoint)
  {
    this.marker = marker;
    this.style = marker.buffer.styles.addStyle("output", null);
    this.adjustPoint = adjustPoint;
    // StyleConstants.setItalic(this.style, true);
  }

  public BufferWriter (Buffer buffer)
  {
    this(buffer.pointMarker, false);
  }

  public synchronized void write(int x)
  {
    boolean mustAdjustPoint
      = adjustPoint && marker.getOffset() == marker.buffer.getDot();
    marker.insert((char) x, 1, style);
    if (mustAdjustPoint)
      marker.buffer.setDot(marker.getOffset());
  }

  public synchronized void write (char[] data, int off, int len)
  {
    if (len == 0)
      return;
    boolean mustAdjustPoint
      = adjustPoint && marker.getOffset() == marker.buffer.getDot();
    marker.insert(new String(data, off, len), style);
    if (mustAdjustPoint)
      marker.buffer.setDot(marker.getOffset());
  }

  public synchronized void flush()
  {
  }

  public synchronized void close()
  {
  }
}

package gnu.emacs;
import java.awt.event.FocusEvent;
import javax.swing.*;

public class Window extends javax.swing.JPanel
  implements java.awt.event.FocusListener
{
  static Window selected;
  
  JTextPane text;
  Modeline modeline;
  Frame frame;
  Buffer buffer;

  Window next;
  Window previous;

  public Window(Buffer buffer)
  {
    this.buffer = buffer;
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    text = new JTextPane(buffer.document);
    modeline = new Modeline(this);
    text.addFocusListener(this);
    add(new JScrollPane(text,
			JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
    add(modeline);
  }

  public static Window getSelected()
  {
    return selected;
  }

  public static void setSelected(Window window)
  {
    if (selected != null)
      selected.unselect();
    window.select();
  }

  public void select()
  {
    selected = this;
    buffer.curPosition = text.getCaret();
  }

  public void unselect()
  {
    buffer.point = buffer.curPosition.getDot();
    buffer.curPosition = null;
    selected = null;
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  public void setBuffer (Buffer buffer)
  {
    this.buffer = buffer;
    text.setDocument(buffer.document);
    modeline.redraw();
    buffer.curPosition = text.getCaret();
    buffer.curPosition.setDot(buffer.point);
  }

  /** Returns the "Emacs value" (1-origin) of point. */
  public int getPoint()
  {
    return 1 + text.getCaret().getDot();
  }

  public void setPoint(int point)
  {
    text.getCaret().setDot(point - 1);
  }

  public Window split (int lines, boolean horizontal)
  {
    return frame.addWindow(buffer, this);
  }

  public void focusGained(FocusEvent e)
  {
    setSelected(this);
    // System.err.println("focus gained for "+this+": "+e);
  }

  public void focusLost(FocusEvent e)
  {
    //System.err.println("focus lost for "+this+": "+e);
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<window on \"");
    sbuf.append(buffer.getName());
    sbuf.append("\" 0x");
    sbuf.append(Integer.toHexString(System.identityHashCode(this)));
    sbuf.append('>');
    return sbuf.toString();
  }
}

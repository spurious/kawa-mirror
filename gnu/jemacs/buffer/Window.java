package gnu.jemacs.buffer;
import java.awt.event.FocusEvent;
import javax.swing.*;
import java.awt.*;

public class Window extends javax.swing.JTextPane
  implements java.awt.event.FocusListener
{
  static Window selected;
  
  Modeline modeline;
  Frame frame;
  Buffer buffer;
  /** The panel that contains this window and the modeline. */
  JPanel panel;
  JScrollPane scrollPane;

  Window next;
  Window previous;

  /** Create new Window.
   * @param buffer the Buffer containing the data.
   */
  public Window(Buffer buffer)
  {
    this(buffer, true);
  }

  public Window(Buffer buffer, boolean wantModeline)
  {
    super(buffer.document);
    this.buffer = buffer;
    setKeymap(buffer.keymap);
    addFocusListener(this);
    if (wantModeline)
      modeline = new Modeline(this);
  }

  /** Create new Window.
   * @param buffer the Buffer containing the data.
   * @param panel the parent panel (which should be empty)
   *  that will contain this Window's JScrollPane and the optional modeline.
   * @param wantModeline true if we should create a mode line
   */
  /*
  public Window(Buffer buffer, JPanel panel, boolean wantModeline)
  {
    this(buffer);
    if (wantModeline)
    wrap(panel, wantModeline);
  }
  */

  public void wrap(JPanel panel)
  {
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    scrollPane = new JScrollPane(this,
                                 JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    panel.add(scrollPane);
    if (modeline != null)
      panel.add(modeline);
    this.panel = panel;
  }

  /** Warp this (and optional modeline) inside a new JPanel. */
  public JPanel wrap()
  {
    JPanel panel = new JPanel();
    wrap(panel);
    return panel;
  }

  /** Get the JPanel containing this Window. */
  public JPanel getPanel()
  {
    return panel;
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

  public static Window getWindow(java.awt.event.ActionEvent event)
  {
    // Maybe use TextAction.getTextComponent instead? FIXME.
    Component component = (Component) event.getSource();
    for (;;)
      {
        if (component instanceof Window)
          return (Window) component;
        component = component.getParent();
      }
  }

  public void select()
  {
    selected = this;
    buffer.curPosition = getCaret();
  }

  public void unselect()
  {
    buffer.point = buffer.curPosition.getDot();
    buffer.curPosition = null;
    selected = null;
  }

  public Frame getFrame()
  {
    return frame;
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  public void setBuffer (Buffer buffer)
  {
    this.buffer = buffer;
    setDocument(buffer.document);
    setKeymap(buffer.keymap);
    modeline.setDocument(buffer.modelineDocument);
    buffer.curPosition = getCaret();
    buffer.curPosition.setDot(buffer.point);
  }

  /** Returns the "Emacs value" (1-origin) of point. */
  public int getPoint()
  {
    return 1 + getCaret().getDot();
  }

  public void setPoint(int point)
  {
    getCaret().setDot(point - 1);
  }

  public Window split (int lines, boolean horizontal)
  {
    return frame.addWindow(buffer, this, horizontal);
  }

  public void delete()
  {
    if (frame.first == this)
      frame.first = next;
    else
      previous.next = next;
    if (next != null)
      next.previous = previous;
    next = null;
    previous = null;
    if (modeline != null)
      panel.remove(modeline);
    panel.remove(scrollPane);
    // FIXME rmeove parent panel if now singleton.
    Container parent = panel.getParent();
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
    sbuf.append(" size: ");
    sbuf.append(getSize());
    sbuf.append('>');
    return sbuf.toString();
  }
}

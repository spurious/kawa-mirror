package gnu.jemacs.buffer;
import java.awt.event.FocusEvent;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;

public class Window extends javax.swing.JTextPane
  implements java.awt.event.FocusListener
{
  Modeline modeline;
  Frame frame;
  Buffer buffer;
  /** The panel that contains this window and the modeline. */
  JPanel panel;
  JScrollPane scrollPane;

  /** Create new Window.
   * @param buffer the Buffer containing the data.
   */
  public Window(Buffer buffer)
  {
    this(buffer, true);
  }

  /** Create new Window.
   * @param buffer the Buffer containing the data.
   * @param wantModeline true if we should create a mode line
   */
  public Window(Buffer buffer, boolean wantModeline)
  {
    super(buffer.document);
    if (wantModeline)
      modeline = new Modeline(this, buffer.modelineDocument);
    setBuffer(buffer);
    addFocusListener(this);
  }

  public Dimension getPreferredScrollableViewportSize()
  {
    Dimension size = panel.getSize();
    if (modeline != null)
      size = new Dimension(size.width,
                       size.height - modeline.getPreferredSize().height);
    return size;
  }

  /** Warp this (and optional modeline) inside a ScrollPane in a new JPanel. */
  public JPanel wrap()
  {
    BorderLayout layout = new BorderLayout();
    JPanel panel = new JPanel(layout);
    scrollPane = new JScrollPane(this,
                                 JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    panel.add(scrollPane, BorderLayout.CENTER);
    if (modeline != null)
      panel.add(modeline, BorderLayout.SOUTH);
    this.panel = panel;
    return panel;
  }

  /** Get the JPanel containing this Window. */
  public JPanel getPanel()
  {
    return panel;
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

  public static Window getSelected()
  {
    return Frame.selectedFrame == null ? null
      : Frame.selectedFrame.selectedWindow;
  }

  public void setSelected()
  {
    Window selected = getSelected();
    if (selected != null && selected.buffer != buffer)
      selected.unselect();

    if (frame != null)
      frame.selectedWindow = this;
    Frame.selectedFrame = frame;
    Buffer.setCurrent(buffer);

    // Change buffer's pointMarker so it follows this Window's Caret.
    buffer.curPosition = getCaret();
    if (buffer.pointMarker.index >= 0)
      buffer.content.freePosition(buffer.pointMarker.index);
    buffer.pointMarker.index = Marker.POINT_POSITION_INDEX;
  }

  public static void setSelected(Window window)
  {
    window.setSelected();
    window.requestFocus();
  }

  void unselect()
  {
    int point = buffer.curPosition.getDot();
    int kind = BufferContent.AFTER_MARK_KIND;
    int index = buffer.content.allocatePosition(point, kind);
    buffer.pointMarker.index = index;
    buffer.curPosition = null;
    // ?? selected = null;
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
    if (this.buffer == buffer)
      return;
    setDocument(buffer.document);
    setKeymap(buffer.keymap);
    if (modeline != null)
      modeline.setDocument(buffer.modelineDocument);

    Window selected = getSelected();
    if (selected == this)
      {
	unselect();
	// Change buffer's pointMarker so it follows this Window's Caret.
	Caret caret = getCaret();
	caret.setDot(buffer.getDot());
	buffer.curPosition = caret;
	if (buffer.pointMarker.index >= 0)
	  buffer.content.freePosition(buffer.pointMarker.index);
	buffer.pointMarker.index = Marker.POINT_POSITION_INDEX;
      }
    this.buffer = buffer;
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

  public void setDot(int offset)
  {
    getCaret().setDot(offset);
  }

  public Window split (int lines, boolean horizontal)
  {
    return split(buffer, lines, horizontal);
  }

  /** Split this window into two.
   * Display Var>buffer</var> in the new window.
   * @return the new Window.
   */
  public Window split (Buffer buffer, int lines, boolean horizontal)
  {
    Window window = new Window(buffer);
    Frame frame = this.frame;
    window.modeline = new Modeline(window, buffer.modelineDocument);
    window.frame = frame;

    JPanel panel = this.getPanel();
    java.awt.Dimension oldSize = panel.getSize();
    java.awt.Container oldParent = panel.getParent();
    oldParent.remove(panel);
    JSplitPane split
      = new JSplitPane(horizontal ? JSplitPane.HORIZONTAL_SPLIT
                       : JSplitPane.VERTICAL_SPLIT,
                       panel, window.wrap());
    split.setDividerSize(2);
    // FIXME - use lines.
    split.setDividerLocation((horizontal ? oldSize.width : oldSize.height) / 2);
    oldParent.add(split);
    oldParent.validate();
    if (this == Window.getSelected())
      this.requestFocus();

    return window;
  }

  /** Return the next/previous Window in the cyclic order of windows.
   * Returns null if this is the last/first window in this Frame. */
  public Window getNextWindow(boolean forwards)
  {
    Container prev = this;
    for (;;)
      {
        Container parent = prev.getParent();
        if (parent == null)
          return null;
        if (parent instanceof JSplitPane)
          {
            JSplitPane split =(JSplitPane) parent;
            if (prev == split.getLeftComponent())
              {
                if (forwards)
                  return getFirstWindow((Container) split.getRightComponent());
              }
            else // prev == split.getRightComponent():
              {
                if (!forwards)
                  return getLastWindow((Container) split.getLeftComponent());
              }
          }
        prev = parent;
      }
  }

  /** Return the next/previous Window in the cyclic order of windows.
   * Returns first/last if this is the last/first window in this Frame. */
  public Window getOtherWindow(boolean forwards)
  {
    Window win = getNextWindow(forwards);
    if (win == null)
      win = forwards ? frame.getFirstWindow() : frame.getLastWindow();
    return win;
  }

  public static Window getContainedWindow(Container cont, boolean last)
  {
    for (;;)
      {
        if (cont instanceof Window)
          return (Window) cont;
        if (cont instanceof JScrollPane)
          cont = (Container) ((JScrollPane) cont).getViewport().getView();
        else if (cont instanceof JFrame)
          cont = ((JFrame) cont).getContentPane();
        else if (cont instanceof JSplitPane)
          {
            JSplitPane split = (JSplitPane) cont;
            cont = (Container)
              (last ? split.getRightComponent() : split.getLeftComponent());
          }
        else
          {
            int count = cont.getComponentCount();
            if (count == 0)
              return null;
            cont = (Container) cont.getComponent(last ? (count - 1) : 0);
          }
      }
  }

  public static Window getFirstWindow(Container cont)
  {
    return getContainedWindow(cont, false);
  }

  public static Window getLastWindow(Container cont)
  {
    return getContainedWindow(cont, true);
  }

  public Window getNextWindowInFrame(int count)
  {
    Window win = this;
    if (count > 0)
      {
        while (--count >= 0)
          win = win.getOtherWindow(true);
      }
    else
      {
        while (++count <= 0)
          win = win.getOtherWindow(false);
      }
    return win;
  }

  public void delete()
  {
    Frame frame = this.frame;
    deleteNoValidate();
    if (frame.getFirstWindow() == null)
      frame.delete();
    else
      frame.validate();
  }

  void deleteNoValidate()
  {
    if (frame.selectedWindow == this)
      {
        Window next = getNextWindowInFrame(1);
        if (frame == Frame.selectedFrame)
          setSelected(next);
        else
          frame.selectedWindow = next;
      }
    if (modeline != null)
      panel.remove(modeline);
    panel.remove(scrollPane);

    // Mow remove the Panel from its parent.
    Container oldParent = panel.getParent();
    if (oldParent instanceof JSplitPane)
      {
        JSplitPane split = (JSplitPane) oldParent;
        Component other;
        if (panel == split.getLeftComponent())
          other = split.getRightComponent();
        else
          other = split.getLeftComponent();
        split.remove(this);
        split.remove(other);
        // In the JSplitPane's parent, replace the JSplitPane by just other 
        Container splitParent = split.getParent();
        if (splitParent instanceof JSplitPane)
          {
            JSplitPane outerSplit = (JSplitPane) splitParent;
            if (split == outerSplit.getLeftComponent())
              outerSplit.setLeftComponent(other);
            else
              outerSplit.setRightComponent(other);
          }
        else
          {
            splitParent.remove(split);
            splitParent.add(other);
          }
      }
    else
      oldParent.remove(panel);

    panel = null;
    scrollPane = null;
    frame = null;
  }

  public void deleteOtherWindows()
  {
    for (Window cur = frame.getFirstWindow(); cur != null; )
      {
        Window next = cur.getNextWindow(true);
        if (cur != this)
          cur.deleteNoValidate();
        cur = next;
      }
    frame.validate();
  }

  public void focusGained(FocusEvent e)
  {
    setSelected();
  }

  public void focusLost(FocusEvent e)
  {
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<window on ");
    if (buffer == null)
      sbuf.append("no buffer");
    else
      {
        sbuf.append('\"');
        sbuf.append(buffer.getName());
        sbuf.append('\"');
      }
    sbuf.append(" 0x");
    sbuf.append(Integer.toHexString(System.identityHashCode(this)));
    sbuf.append('>');
    return sbuf.toString();
  }
}

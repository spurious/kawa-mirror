package gnu.jemacs.buffer;
import javax.swing.*;

public class Frame extends javax.swing.JFrame
{
  public Window first;
  JPanel contents;

  static int counter;
  int id = ++counter;

  public static String defaultName ()
  {
    return "Emacs";
  }

  public Frame ()
  {
    super(defaultName());
    setSize(400, 300);
  }

  /** Make a new window on BUFFER and link it after BEFORE.
   * If BEFORE is null (untested), make the new window be first. */
  public Window
  addWindow (Buffer buffer, Window before, boolean horizontal)
  {
    Window window = new Window(buffer);
    Frame frame = before.frame;
    window.modeline = new Modeline(window);
    if (frame.first == null)
      {
	window.next = window;
	window.previous = window;
	frame.first = window;
      }
    else
      {
	if (before == null)
	  {
	    before = frame.first.previous;
	    frame.first = window;
	  }
	before.next = window;
	window.previous = before;
	window.next = before.next;
	before.next = window;
      }
    window.frame = frame;

    // We will re-use the old Window's panel as a parent for both
    // the old and new Windows.
    JPanel panel = before.getPanel();
    if (before.modeline != null)
      panel.remove(before.modeline);
    if (before.scrollPane != null)
      panel.remove(before.scrollPane);
    else
      panel.remove(before);

    // New add both windows (old and new) to the panel.
    // Both windows have to be re-wrapped.
    int axis = horizontal ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS;
    panel.setLayout(new BoxLayout(panel, axis));
    panel.add(before.wrap());
    panel.add(window.wrap());
    panel.validate();
    return window;
  }

  /** Add a new window as the last window. */
  public Window addWindow (Buffer buffer)
  {
    return addWindow(buffer, first == null ? null : first.previous, false);
  }

  public Frame (Buffer buffer)
  {
    Window first = new Window(buffer, true);
    first.frame = this;
    contents = first.wrap();
    getContentPane().add(contents);
    setSize(600, 400);
    setVisible(true);
    if (Window.selected == null)
      Window.setSelected(first);
  }

  public String ask(String prompt)
  {
    String result = JOptionPane.showInputDialog(this, prompt);
    if (result == null)
      throw new CancelledException();
    return result;
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<frame #");
    sbuf.append(id);
    sbuf.append(" size: ");
    sbuf.append(getSize());
    sbuf.append(" preferred: ");
    sbuf.append(getPreferredSize());
    sbuf.append('>');
    return sbuf.toString();
  }
}

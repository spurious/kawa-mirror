package gnu.emacs;
import javax.swing.*;

public class Frame extends javax.swing.JFrame
{
  public Window first;
  JPanel contents;

  public static String defaultName ()
  {
    return "Emacs";
  }

  public Frame ()
  {
    super(defaultName());
    setSize(400, 300);
  }

  /** Make a new window on BUFFER and link it after BEFORE. */
  public Window addWindow (Buffer buffer, Window before)
  {
    Window window = new Window(buffer);
    if (first == null)
      {
	window.next = window;
	window.previous = window;
	first = window;
      }
    else
      {
	if (before == null)
	  {
	    before = first.previous;
	    first = window;
	  }
	before.next = window;
	window.previous = before;
	window.next = before.next;
	before.next = window;
      }
    window.frame = this;
    contents.add(window);
    //    System.err.println("layout:"+getContentPane().getLayout().getClass());
    pack();
    return window;
  }

  /** Add a new window as the last window. */
  public Window addWindow (Buffer buffer)
  {
    return addWindow(buffer, first == null ? null : first.previous);
  }

  public Frame (Buffer buffer)
  {
    contents = new JPanel();
    contents.setLayout(new BoxLayout(contents, BoxLayout.Y_AXIS));
    getContentPane().add(contents);
    Window window = addWindow(buffer);
    setSize(400, 300);
    setVisible(true);
  }
}

package gnu.jemacs.buffer;
import javax.swing.*;

public class Frame extends javax.swing.JFrame
{
  static Frame selectedFrame;
  Window selectedWindow;

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
    if (selectedFrame == null)
      selectedFrame = this;
    setTitle("JEmacs");
  }

  public Frame (Buffer buffer)
  {
    Window win = new Window(buffer, true);
    win.frame = this;
    contents = win.wrap();
    getContentPane().add(contents);
    setSize(600, 400);
    setVisible(true);
    Window.setSelected(win);
    setTitle("JEmacs");
  }

  public void delete()
  {
    for (Window cur = getFirstWindow(); cur != null; )
      {
        Window next = cur.getNextWindow(true);
        cur.deleteNoValidate();
        cur = next;
      }
    contents = null;
    dispose();
    if (this == selectedFrame)
      selectedFrame = null;
  }

  public boolean isLive()
  {
    return contents != null;
  }

  public Window getFirstWindow()
  {
    return Window.getFirstWindow(this);
  }

  public Window getLastWindow()
  {
    return Window.getLastWindow(this);
  }

  public static Frame getSelectedFrame()
  {
    return selectedFrame;
  }

  public Window getSelectedWindow()
  {
    return selectedWindow;
  }

  public Window otherWindow(int count)
  {
    return selectedWindow.getNextWindowInFrame(count);;
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

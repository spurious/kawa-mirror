// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;
import javax.swing.*;

/** An Emacs frame (EFrame) implemented using the Swing toolkits. */

public class SwingFrame extends EFrame
{
  javax.swing.JFrame jframe;
  JMenuBar menuBar;
  JPanel contents;

  public SwingFrame ()
  {
    super();
  }

  public SwingFrame (Buffer buffer)
  {
    this(new SwingWindow(buffer, true));
  }

  public SwingFrame (SwingWindow window)
  {
    super(window);
    contents = window.wrap();
    jframe = new JFrame(defaultName());
    jframe.getContentPane().add(contents);
    jframe.setSize(600, 400);
    jframe.setVisible(true);
    jframe.setTitle("JEmacs");
    menuBar = new JMenuBar();
    jframe.setJMenuBar(menuBar);
  }

  public boolean isLive()
  {
    return contents != null;
  }

  public void validate ()
  {
    jframe.validate();
  }

  public void delete()
  {
    super.delete();
    contents = null;
    jframe.dispose();
  }

  public String ask(String prompt)
  {
    String result = JOptionPane.showInputDialog(jframe, prompt);
    if (result == null)
      throw new CancelledException();
    return result;
  }

  public void setMenuBar (Menu menu)
  {
    menuBar.removeAll();
    // a menubar contain a list of menus, stored inside a single menu
    while (menu.getMenuComponentCount() > 0)
      menuBar.add(menu.getMenuComponent(0));
    menuBar.updateUI();
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<frame #");
    sbuf.append(id);
    sbuf.append(" size: ");
    sbuf.append(jframe.getSize());
    sbuf.append(" preferred: ");
    sbuf.append(jframe.getPreferredSize());
    sbuf.append('>');
    return sbuf.toString();
  }
}


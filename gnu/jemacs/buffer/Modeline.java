package gnu.jemacs.buffer;
import java.awt.event.FocusEvent;
import javax.swing.*;
import java.awt.*;

public class Modeline extends javax.swing.JTextPane
{
  Window window;

  public Modeline(Window window)
  {
    super();
    this.window = window;
    redraw();
  }

  public void redraw()
  {
    setText("---JEmacs: " + window.buffer.getName() + " ---");
  }

  public Dimension getMinimumSize() { return getPreferredSize(); }
  public Dimension getMaximumSize() { return getPreferredSize(); }
}

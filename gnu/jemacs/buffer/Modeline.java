package gnu.jemacs.buffer;
import java.awt.event.FocusEvent;
import javax.swing.*;
import java.awt.*;

// Any point to this now?
public class Modeline extends javax.swing.JTextPane
{
  Window window;

  public Modeline(Window window)
  {
    super(window.buffer.modelineDocument);
    this.window = window;
  }

  public Dimension getMinimumSize() { return getPreferredSize(); }
  public Dimension getMaximumSize() { return getPreferredSize(); }
}

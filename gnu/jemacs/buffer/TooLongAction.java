package gnu.jemacs.buffer;
import javax.swing.*;
import javax.swing.text.*;

/** A pseudo-action to wrap an integer.
 * Used by BufferKeymap.lookupKey. */

public class TooLongAction extends javax.swing.AbstractAction
{
  int maxValid;

  public TooLongAction(int maxValid)
  {
    //super(Integer.toString(maxValid));
    this.maxValid = maxValid;
  }

  public int getMaxValid() { return maxValid; }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    // Should never happen - ignore.
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
  }

}

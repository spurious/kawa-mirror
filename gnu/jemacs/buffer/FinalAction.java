package gnu.jemacs.buffer;
import gnu.mapping.Procedure;
import javax.swing.text.*;
import javax.swing.*;

/** A wrapper around an action.
 * This first also clears the buffer's pending keys. */

public class FinalAction extends javax.swing.text.TextAction
{
  Action action;

  public FinalAction (Action action)
  {
    super(null);
    this.action = action;
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    action.actionPerformed(event);
  }
}

package gnu.jemacs.buffer;
import javax.swing.text.*;
import javax.swing.*;

/** This Action is performed when a prefix key is typed. */

public class PrefixAction extends javax.swing.text.TextAction
{
  KeyStroke prefix;
  Keymap next;

  public PrefixAction (KeyStroke prefix, Keymap next)
  {
    super(next.getName());
    this.next = next;
    this.prefix = prefix;
  }

  public Keymap getKeymap()
  {
    return next;
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    Buffer buffer = Window.getWindow(event).buffer;
    if (prefix != null)
      buffer.keymap.pushPrefix(prefix);
  }
}

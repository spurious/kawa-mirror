package gnu.jemacs.buffer;

/** An Action to insert the typed character into a buffer. */

public class InsertAction extends javax.swing.text.TextAction
{
  public InsertAction(String name)
  {
    super(name);
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    int count = 1;  // Get C-u prefix.  FIXME.
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    String command = event.getActionCommand();
    char ch = command.charAt(0);
    // We specifically want to suppress inserting BackSpace.
    // The problem is that the backspace key-binding is triggered
    // for the key-press, but we *also* get a key-typed actions, which
    // ends up here.  There is probably a cleaner solution ...
    if (ch >= ' ' || ch == '\n' || ch == '\t'
	|| this != BufferKeymap.defaultInsertAction)
      buffer.insert(ch, count, buffer.inputStyle);
  }
}

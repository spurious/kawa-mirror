package gnu.jemacs.buffer;
import gnu.mapping.Procedure;

/** An Action that causes a Procedure to be excuted. */

public class Command extends javax.swing.text.TextAction
{
  Object command;

  Command (Object command, String name)
  {
    super(name);
    this.command = command;
  }

  Command (Procedure command)
  {
    super(command.getName());
    this.command = command;
  }

  public final Object getCommand()
  {
    return command;
  }

  public static void perform(Object command)
  {
    try
      {
	if (command instanceof String)
	  command = gnu.jemacs.lang.Symbol.getBinding((String) command).getProcedure();
        ((Procedure) command).apply0();
      }
    catch (CancelledException ex)
      {
        // Do nothing.
      }
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    perform(command);
  }
}


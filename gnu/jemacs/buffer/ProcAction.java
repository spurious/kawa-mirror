package gnu.jemacs.buffer;
import gnu.mapping.Procedure;

/** An Action that causes a Procedure to be excuted. */

public class ProcAction extends javax.swing.text.TextAction
{
  Procedure proc;

  ProcAction (Procedure proc, String name)
  {
    super(name);
    this.proc = proc;
  }

  ProcAction (Procedure proc)
  {
    super(proc.getName());
    this.proc = proc;
  }

  public final Procedure getProcedure()
  {
    return proc;
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    try
      {
        proc.apply0();
      }
    catch (CancelledException ex)
      {
        // Do nothing.
      }
  }
}


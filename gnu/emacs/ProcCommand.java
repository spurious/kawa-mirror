package gnu.emacs;
import gnu.mapping.Procedure;

public class ProcCommand extends javax.swing.text.TextAction
{
  Procedure proc;

  ProcCommand (Procedure proc, String name)
  {
    super(name);
    this.proc = proc;
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    System.err.println("(perform "+proc.getName()+")");
    proc.apply0();
  }
}


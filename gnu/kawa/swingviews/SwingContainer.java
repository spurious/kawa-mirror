package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import javax.swing.*;
import java.awt.event.*;
import gnu.mapping.Procedure;

public class SwingContainer extends JComponent
implements ViewContainer
{
  public Object addButton (Button model)
  {
    SwingButton button = new SwingButton(model);
    this.add(button);
    return button;
  }

  /*
  public Object addMenuBar (MenuBar model)
  {
    SwingButton button = new SwingMenuBar(model);
    this.add(button);
    return button;
  }
  */

  public static ActionListener makeActionListener (Object command)
  {
    if (command instanceof ActionListener)
      return (ActionListener) command;
    return new ProcActionListener((Procedure) command);
  }
}

class ProcActionListener implements ActionListener
{
  Procedure proc;

  public ProcActionListener (Procedure proc) { this.proc = proc; }

  public void actionPerformed (ActionEvent e)
  {
    try
      {
	proc.apply0();
      }
    catch (Throwable ex)
      {
	throw new gnu.mapping.WrappedException(ex);
      }
  }

}

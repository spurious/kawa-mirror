package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import javax.swing.*;

public class SwingButton
extends JButton
			      /*  implements ButtonModel*/
{
  /*
  Button model;

  // State needed by Swing's ButtonModel but may not be really "model".
  boolean armed;
  boolean pressed;
  boolean rollover;
  */

  public SwingButton (Button model)
  {
    setModel(new SwModel(model));
    Object action = model.getAction();
    if (action != null)
      addActionListener(SwingContainer.makeActionListener(action));
    setText(model.getLabel());
  }
}

class SwModel extends DefaultButtonModel
{
  Button model;

  public SwModel (Button model)
  {
    this.model = model;
    setActionCommand(model.getLabel());
  }

  /*
  // ButtonModel methods
  public boolean isEnabled () { System.err.println("SwB.sisEnabled called");return ! model.isDisabled(); }
  public void setEnabled (boolean b) { System.err.println("SwB.setEnabled called");model.setDisabled(!b); }
  */
  /*
  public boolean isArmed () { System.err.println("SwB.isArmed called");return armed; }
  public void setArmed (boolean armed) { System.err.println("SwB.setArmed called");this.armed = armed; }
  public void setPressed (boolean pressed) { System.err.println("SwB.setPressed called");this.pressed = pressed; }
  public void setRollover (boolean rollover) { System.err.println("SwB.setRollover called");this.rollover = rollover; }
  public boolean isPressed () { System.err.println("SwB.uisPressed called");return pressed; }
  public boolean isRollover () { System.err.println("SwB.isRollover called");return rollover; }
  public void setGroup (ButtonGroup group)
  { 
    System.err.println("SwB.setGroup called");
throw new UnsupportedOperationException();
  }
  */

}

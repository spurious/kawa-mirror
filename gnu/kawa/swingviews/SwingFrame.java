package gnu.kawa.swingviews;
import gnu.kawa.models.*;
import java.awt.Component;
import java.awt.Container;
import javax.swing.*;
import gnu.lists.*;

public class SwingFrame extends JFrame
implements ViewContainer
{
  public Object addButton (Button model)
  {
    System.err.flush();
    SwingButton button = new SwingButton(model);
    getContentPane().add(button);
    return button;
  }

  public SwingFrame (String title,
		     javax.swing.JMenuBar menubar,
		     Object contents)
  {
    JFrame fr = this;
    if (title != null)
      fr.setTitle(title);
    if (menubar != null)
      fr.setJMenuBar(menubar);
    Container pane = getContentPane();
    pane.setLayout(new BoxLayout(pane, BoxLayout.X_AXIS));
    add(contents);
    pack();
    show();
  }

  private void add (Object contents)
  {
    if (contents instanceof AbstractSequence)
      {
	AbstractSequence seq = (AbstractSequence) contents;
	for (int iter = seq.startPos();  (iter = seq.nextPos(iter)) != 0; )
	  add(seq.getPosPrevious(iter));
      }
    else if (contents instanceof Viewable)
      ((Viewable) contents).makeView(this);
    else if (contents instanceof Paintable)
      getContentPane().add(new SwingPaintable((Paintable) contents));
    else
      getContentPane().add((Component) contents);
  }
}

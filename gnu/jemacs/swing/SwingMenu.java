package gnu.jemacs.swing;
import java.awt.event.*;
import javax.swing.*;
import gnu.lists.*;
import gnu.mapping.Procedure;
import gnu.jemacs.buffer.Command;
import gnu.jemacs.buffer.EMenu;
import gnu.jemacs.lang.*;
import java.util.*;

/**
 * This manages a menu (for menubars or popup menus).
 * @author Simon Josefsson <jas@pdc.kth.se> (orginal contribution)
 */

public class SwingMenu extends JMenu implements EMenu
{
  public SwingMenu ()
  {
    super();
  }
    
  public SwingMenu (LList menu)
  {
    super();
    setMenu (menu);
  }
    
  public void setMenu (LList menu)
  {
    java.util.Enumeration e = menu.elements();
    for (int i = 0;  e.hasMoreElements(); i++)
      {
	Object item = e.nextElement();
	if (item == null)
	  {
	    this.add(Box.createHorizontalGlue());
	  }
	else if (item instanceof FString)
	  {
	    if (i == 0)
		this.setText(item.toString());
	    else
	      // FIXME handle different type of separators
	      this.addSeparator();
	  }
	else if (item instanceof FVector)
	  {
	    FVector menuEntry = (FVector) item;

	    if (menuEntry.get(0) instanceof FString)
	      {
		FString txt = (FString) menuEntry.get(0);
		Object proc = menuEntry.get(1);

		// FIXME handle all possible keywords

		JMenuItem menuItem = new MenuItem (txt.toString(), proc);
		this.add(menuItem);
	      }
	  }
	else if (item instanceof JComponent)
	  this.add((JComponent) item);
	else if (item instanceof LList)
	  {
	    // FIXME don't create new objects, keep it within this
	    SwingMenu tmp = new SwingMenu((LList) item);
	    this.add(tmp);
	  }
      }
  }

}

class MenuItem extends JMenuItem implements java.awt.event.ActionListener
{
  Object command;

  public MenuItem(String text, Object command)
  {
    super(text);
    this.command = command;
    this.addActionListener(this);
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    JMenuItem source = (JMenuItem) event.getSource();
    Command.perform(command);
  }
}

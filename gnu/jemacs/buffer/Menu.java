package gnu.jemacs.buffer;
import java.awt.event.*;
import javax.swing.*;
import gnu.kawa.util.*;
import gnu.mapping.Procedure;
import gnu.jemacs.lang.*;
import java.util.*;

/**
 * This manages a menu (for menubars or popup menus).
 * @author Simon Josefsson <jas@pdc.kth.se> (orginal contribution)
 */

public class Menu extends JMenu implements ActionListener
{
  Hashtable menuEntries = new Hashtable();

  public Menu ()
  {
    super();
  }
    
  public Menu (Object menu)
  {
    super();
    setMenu (menu);
  }
    
  public void setMenu (Object menu)
  {
    if (! (menu instanceof LList))
      return;

    Sequence menuList = (LList) menu;
	
    if (menuList.get(0) instanceof FString)
      {
	FString name = (FString) menuList.get(0);
	this.setText(name.toString());
      }

    for (int i = 0; i < menuList.length(); i++)
      {
	if (menuList.get(i) == null)
	  {
	    this.add(Box.createHorizontalGlue());
	  }
	else if (menuList.get(i) instanceof FString)
	  {
	    // FIXME handle different type of separators
	    this.addSeparator();
	  }
	else if (menuList.get(i) instanceof FVector)
	  {
	    FVector menuEntry = (FVector) menuList.get(i);

	    if (menuEntry.get(0) instanceof FString)
	      {
		FString txt = (FString) menuEntry.get(0);
		String proc = (String) menuEntry.get(1);

		// FIXME handle all possible keywords

		JMenuItem menuItem = new JMenuItem (txt.toString());
		menuEntries.put(txt.toString(), proc);
		menuItem.addActionListener(this);
		this.add(menuItem);
	      }
	  }
	else if (menuList.get(i) instanceof LList)
	  {
	    // FIXME don't create new objects, keep it within this
	    Menu tmp = new Menu(menuList.get(i));
	    this.add(tmp);
	  }
      }
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    JMenuItem source = (JMenuItem) event.getSource();
    String procname = (String) menuEntries.get(source.getText());
      
    if (gnu.jemacs.lang.Symbol.isBound (procname))
      {
	Procedure proc = gnu.jemacs.lang.Symbol.getBinding(procname).getProcedure();
	try
	  {
	    proc.apply0();
	  } catch (CancelledException ex) {
	    // Do nothing.
	  }
      }
  }
}

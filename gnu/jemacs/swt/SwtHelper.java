//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

/**
 * A Class to act as a layer between SwtJemacs and SWT. 
 * SWT requires that (almost) all calls to widget methods and
 * constructors happen in the GUI event loop thread. On the other hand
 * it should be possible in JEmacs/Kawa to create new threads and call functions 
 * that may, in turn, call SwtJemacs. Therefore we must have this layer between 
 * SwtJemacs and SWT. Each method in this class will make sure it is executed in 
 * the GUI loop thread. 
 * 
 * @author Christian Surlykke
 *         25-08-2004
 */
public class SwtHelper
{

  /**
   * Calls menubar.dispose()
   * 
   * @param menubar The menu to dispose
   */
  public static void dispose(final Menu menubar)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Resultable(){ public void run() { dispose(menubar);}});
    }
    else 
    {
      menubar.dispose();
    }
  }

  /**
   * Creates a new menu bar for a shelln
   * 
   * @param shell
   * @param bar
   * @return
   */
  public static Menu newMenu(final Shell shell, final int bar)
  {
    if (Thread.currentThread() != getDisplay().getThread()) 
    {
      Resultable res = new Resultable() { public void run() { result = newMenu(shell, bar);}};
      getDisplay().syncExec(res);
      return (Menu) res.result;
    }
    else
    {
      return new Menu(shell, bar);
    }
  }

  /**
   * Creates a new Menu inside a MenuItem.
   * 
   * @param menuItem
   * @return
   */
  public static Menu newMenu(final MenuItem menuItem)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newMenu(menuItem);}};
      getDisplay().syncExec(res);
      return (Menu) res.result;
    }
    else 
    {
      return new Menu(menuItem);
    }
  }

  /**
   * Creates a new MenuItem.
   * 
   * @param parent
   * @param style
   * @param text
   * @return
   */
  public static MenuItem newMenuItem(final Menu parent, final int style, final String text, final SelectionListener selectionListener)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newMenuItem(parent, style, text, selectionListener);}};
      getDisplay().syncExec(res);
      return (MenuItem) res.result;
    }
    else
    {
      MenuItem menuItem = new MenuItem(parent, style);
      if (text != null)
      {
        menuItem.setText(text);
      }
      if (selectionListener != null)
      {
        menuItem.addSelectionListener(selectionListener);
      }
      return menuItem;
    }
  }

  /**
   * Sets a menu bar for a shell. The menu must have been created 
   * with the shell as parent
   * 
   * @param shell
   * @param menubar
   */
  public static void setMenuBar(final Shell shell, final Menu menubar)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Runnable() {public void run() {setMenuBar(shell, menubar);}});
    }
    else
    {
      shell.setMenuBar(menubar);
    }
  }

  /**
   * @param menuItem
   * @param subMenu
   */
  public static void setMenu(final MenuItem menuItem, final Menu subMenu)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      getDisplay().syncExec(new Runnable() {public void run() {setMenu(menuItem, subMenu);}});
    }
    else
    {
      menuItem.setMenu(subMenu);
    }
  }

  /**
   * @param display
   * @param layout
   * @return
   */
  public static Shell newShell(final Display display, final FillLayout layout)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newShell(display, layout);}};
      getDisplay().syncExec(res);
      return (Shell) res.result;
    }
    else
    {
      Shell shell = new Shell(display);
      if (layout != null)
      {
        shell.setLayout(layout);
      }
      shell.open();
      return shell;
    }
  }

  /**
   * Creates a StyledText instance with a given content, and an SwtWindow which will be installed as
   * VerifyKeylistener, FocusListener, KeyListener and Mouselistener
   * 
   * @param parent
   * @param style
   * @param swtBuffer
   * @param window
   * @return
   */
  public static StyledText newStyledText(final Composite parent, final int style, final StyledTextContent styledTextContent, final SwtWindow swtWindow)
  {
    if (Thread.currentThread() != getDisplay().getThread())
    {
      Resultable res = new Resultable() {public void run() {result = newStyledText(parent, style, styledTextContent, swtWindow);}};
      getDisplay().syncExec(res);
      return (StyledText) res.result;
    }
    else {
      StyledText styledText = new StyledText(parent,  style);
      styledText.setContent(styledTextContent);
      styledText.addVerifyKeyListener(swtWindow);
      styledText.addFocusListener(swtWindow);
      styledText.addKeyListener(swtWindow);
      styledText.addMouseListener(swtWindow);
      return styledText;
    }
  }

  private static Display display = null;

  
  public static Display getDisplay()
  {
    if (SwtHelper.display == null)
    {
      Runnable guiLoop = new Runnable()
      {
        public void run()
        {
          SwtHelper.display = new Display();
          
          while (!SwtHelper.display.isDisposed ()) 
          {
            try
            {
              if (!SwtHelper.display.readAndDispatch ())
              {
                SwtHelper.display.sleep ();
              }
            }
            catch (Exception e) 
            {
              e.printStackTrace();
            }
          }
          SwtHelper.display.dispose ();
        }
      };
      
      (new Thread(guiLoop)).start();
      
      while (SwtHelper.display == null)
      {
        try
        {
          Thread.sleep(20);
        }
        catch (InterruptedException ie)
        {
        }
      }
    }
  
    return SwtHelper.display;
  }
  
  /**
   * A variant of Runnable to be used in Display.syncExec when the caller 
   * wants to retrive the result. 
   * 
   */
  public static abstract class Resultable implements Runnable
  {
    public Object result;
  }
  
}

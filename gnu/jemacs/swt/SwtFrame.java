//Copyright (c) 2004 Christian Surlykke.
//This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.swt;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

import gnu.jemacs.buffer.Buffer;
import gnu.jemacs.buffer.EFrame;
import gnu.lists.LList;

/**
 * @author Christian Surlykke
 *         11-07-2004
 */
public class SwtFrame extends EFrame
{

  private Shell shell;
  private Display display;
  private SwtWindow swtWindow;
  private Menu menu;
  
  public SwtFrame ()
  {
    super();
  }

  public SwtFrame (Buffer buffer)
  {
    this(new SwtWindow(buffer, true));
  }

  public SwtFrame (SwtWindow window)
  {
    super(window);
    this.swtWindow = window;

    Runnable uiThread = new Runnable() 
    {
      public void run()
      {
          display = new Display ();
          shell = new Shell (display);
          shell.setLayout(new FillLayout());
          swtWindow.getReadyToShow(shell);
          shell.open ();
          
          while (!shell.isDisposed ()) 
          {
            try
            {
              if (!display.readAndDispatch ())
              {
                display.sleep ();
              }
            }
            catch (Exception e) 
            {
              e.printStackTrace();
            }
          }
          display.dispose ();
      }
    };
    
    Thread thread = new Thread(uiThread);
    thread.start();
  }
  
  /**
   * @see gnu.jemacs.buffer.EFrame#isLive()
   */
  public boolean isLive()
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see gnu.jemacs.buffer.EFrame#ask(java.lang.String)
   */
  public String ask(String prompt)
  {
    Shell shell = new Shell();
    InputDialog inputDialog = new InputDialog(shell, "Jemacs input window", prompt, "", null);
    inputDialog.open();
    String result = inputDialog.getValue();
    inputDialog.close();
    
    return result;
  }

  /**
   * @return
   */
  public Shell getShell()
  {
    return this.shell;
  }

  /**
   * @see gnu.jemacs.buffer.EFrame#setMenu(gnu.lists.LList)
   */
  public void setMenu(LList menu)
  {
    // TODO
  }

}

// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;

public abstract class EFrame
{
  public static EFrame selectedFrame;
  public EWindow selectedWindow;
    
  static int counter;
  protected int id = ++counter;
  public EWindow firstWindow;

  public static String defaultName ()
  {
    return "Emacs";
  }

  public EFrame ()
  {
    if (selectedFrame == null)
      selectedFrame = this;
  }

  public EFrame (EWindow win)
  {
    win.frame = this;
    firstWindow = win;
    win.nextWindow = win;
    win.prevWindow = win;
    EWindow.setSelected(win);
  }

  public abstract void setMenuBar (Menu menu);

  public void validate ()
  {
  }

  public void delete()
  {
    for (;;)
      {
	EWindow win = firstWindow;
	if (win == null)
	  break;
        win.deleteNoValidate();
      }
    if (this == selectedFrame)
      selectedFrame = null;
  }

  public abstract boolean isLive();

  public EWindow getFirstWindow()
  {
    return firstWindow;
  }

  public EWindow getLastWindow()
  {
    return firstWindow.prevWindow;
  }

  public static EFrame getSelectedFrame()
  {
    return selectedFrame;
  }

  public EWindow getSelectedWindow()
  {
    return selectedWindow;
  }

  public EWindow otherWindow(int count)
  {
    return selectedWindow.getNextWindowInFrame(count);
  }

  public abstract String ask(String prompt);

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<frame #");
    sbuf.append(id);
    sbuf.append('>');
    return sbuf.toString();
  }
}

package gnu.kawa.echo2;
import gnu.kawa.models.*;
import gnu.mapping.WrappedException;

public class Echo2Window extends nextapp.echo2.app.Window
                         implements gnu.kawa.models.Window
{
  boolean registered;
  Object content;
  Echo2Display display;

  public Display getDisplay ()
  {
    return display;
  }

  public void setMenuBar (Object menubar)
  {
    // Error? warning? ignore?
  }

  public void setContent (Object content)
  {
    if (registered)
      setContent(Echo2Display.asContentPane(content));
    else
      this.content = content;
  }

  public void open ()
  {
    try
      {
        Echo2Servlet.create(this);
      }
    catch (Throwable ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }
}

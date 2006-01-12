package gnu.kawa.echo2;
import nextapp.echo2.webcontainer.*;
import nextapp.echo2.app.ApplicationInstance;

public class Echo2AppInstance extends ApplicationInstance
{
  Echo2Window mainWindow;

  public nextapp.echo2.app.Window init ()
  {
    Echo2Window window = this.mainWindow;
    window.registered = true;
    if (window.content != null)
      window.setContent(window.content);
    return window;
  }
}

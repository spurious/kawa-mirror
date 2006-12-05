package gnu.kawa.echo2;
import gnu.kawa.servlet.*;
import gnu.mapping.*;
import gnu.expr.*;
import nextapp.echo2.webcontainer.*;
import nextapp.echo2.app.ApplicationInstance;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.kawa.models.Display;

import nextapp.echo2.webrender.*;

public class Echo2Servlet
extends WebContainerServlet
{
  Class modClass;
  String name;

  static final String MAIN_WINDOW_KEY = "kawa-echo2-window";

  KawaServlet script;

  // As long as Echo2Display doesn't have connection-specific state, we
  // can use field (even a static field).  Otherwise, we could save the
  // Display in the user session.
  private Echo2Display display;
  public Echo2Display getDisplay () { return display; }
  public void setDisplay (Echo2Display display) { this.display = display; }
  
  public Echo2Servlet (String name, Class modClass)
  {
    this.name = name;
    this.modClass = modClass;
  }

  public static void create (Echo2Window window)
    throws javax.servlet.ServletException, java.io.IOException
  {
    ServletCallContext ctx = (ServletCallContext) CallContext.getInstance();
    HttpSession session = ctx.request.getSession(true);
    session.putValue(MAIN_WINDOW_KEY, window);
    ApplicationInstance appInstance = ApplicationInstance.getActive();

    if (appInstance == null)
      {
        ModuleInfo minfo = ctx.minfo;
        Echo2Servlet servlet;
        if (ctx.servlet instanceof Echo2Servlet)
          servlet = (Echo2Servlet) ctx.servlet;
        else
          {
            servlet = new Echo2Servlet (minfo.sourceAbsPath, minfo.moduleClass);
            KawaServlet script = (KawaServlet) ctx.servlet;
            servlet.script = script;
            servlet.setDisplay(window.display);
            servlet.init(script.config);
            //ServletPrinter out = (ServletPrinter) ctx.consumer;
            // Following will run run newApplicationInstance
            ctx.servlet = servlet; // FIXME ?????
          }
        servlet.process(ctx.request, ctx.response);
      }
    //else      ((Echo2AppInstance) appInstance).setMainWindow(window);
  }

  public String getServletName()
  {
    return name;
  }

  public ApplicationInstance newApplicationInstance ()
  {
    ServletCallContext ctx = (ServletCallContext) CallContext.getInstance();
    HttpSession session = ctx.request.getSession(true);
    Echo2Window window = (Echo2Window) session.getValue(MAIN_WINDOW_KEY);
    Echo2AppInstance appInstance = new Echo2AppInstance();
    if (window == null)
      {
        try
          {
            script.process(ctx);
          }
        catch (Throwable ex)
          {
            throw WrappedException.wrapIfNeeded(ex);
          }
        window = (Echo2Window) session.getValue(MAIN_WINDOW_KEY);
        //if (window == null) ...
      }
    appInstance.mainWindow = window;
    //appInstance.display = getDisplay();
    /*
    if (ctx instanceof ServletCallContext
        && ((ServletCallContext) ctx).inModuleBody())
      {
        // We're called (indirectly) from KawaServlet.process.
        // which has called 'application', so we don't need to run the
        // the ModuleBody.
        // (This happens the first time such as servlet is called.)
      }
    else
      {
        try
          {
    if (false)
      script.process(ctx);
            Object modInstance = modClass.newInstance();
            ApplicationInstance.setActive(appInstance);
            if (modInstance instanceof ModuleBody)
              ((ModuleBody) modInstance).run(ctx);
            ApplicationInstance.setActive(null);
          }
        catch (Throwable ex)
          {
            throw WrappedException.wrapIfNeeded(ex);
          }
      }
    */
    return appInstance;
  }

  protected void process (HttpServletRequest req, HttpServletResponse res)
    throws ServletException, java.io.IOException
  {
    Object saveDisplay = Display.myDisplay.get(null);
    Display.myDisplay.set(getDisplay());
    super.process(req, res);
    Display.myDisplay.set(saveDisplay);
  }

  public void destroy ()
  {
    super.destroy();
  }
}

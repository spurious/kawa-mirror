package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.IOException;

public abstract class KawaServlet
extends HttpServlet implements CpsMethodContainer
{
  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException
  {
    response.setContentType("text/html");
    ServletCallContext ctx = new ServletCallContext();
    ctx.request = request;
    ctx.response = response;
    ctx.servlet = this;
    ctx.values = Values.noArgs;
    OutPort out = new OutPort(response.getOutputStream());
    out.println("<html>");
    ctx.consumer = new XMLPrinter(out);
    apply(ctx);
    ctx.run();
    out.println("</html>");
  }

  public final void apply(CpsMethodProc proc, CallContext context)
  {
    apply(context);
  }

  public abstract void apply(CallContext context);
}

class ServletCallContext extends CallContext
{
  public HttpServletRequest request;
  public HttpServletResponse response;
  public HttpServlet servlet;
}

